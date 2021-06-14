module Graphs (
) where

import Control.Lens ((^.), (%%~))
import Control.Monad.Haskey
import Control.Monad.Reader
import           Control.Monad         (forever)
import qualified System.IO.Streams     as Streams

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.BTree.Impure (Tree)
import Data.BTree.Primitives (Value)
import Data.Binary (Binary)
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.BTree.Impure as B
import Test.QuickCheck

import Database.Haskey.Alloc.Concurrent (Root)

import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Generic.Random (genericArbitrary, uniform)
import qualified Database.MySQL.BinLog as BinLog
import qualified Database.MySQL.Base as MySQL
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (restoreM, MonadBaseControl (liftBaseWith))

data Triple = Triple Text Text Text
 deriving (Generic, Show)
instance Binary Triple
instance Value Triple
instance Arbitrary Triple where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

data Schema = Schema {
    objects :: Tree Text Triple
  , predicates :: Tree Text Triple
  , subjects :: Tree Text Triple
} deriving (Generic, Show)
instance Binary Schema
instance Value Schema
instance Root Schema

emptySchema :: Schema
emptySchema = Schema B.empty B.empty B.empty

insertTriple :: AllocM n => Schema -> Triple -> n Schema
insertTriple schema triple@(Triple o p s) = do
    _ <- #objects %%~ B.insert o triple $ schema
    _ <- #predicates %%~ B.insert p triple $ schema
    _ <- #subjects %%~ B.insert s triple $ schema
    pure schema

insertSomeTriples :: App ()
insertSomeTriples = do
    triples <- liftIO $ sample' (arbitrary :: Gen Triple)
    transact_ $ \schema ->
        foldlM insertTriple schema triples
        >>= commit_

queryAllTriples :: AllocReaderM n => Schema -> n [(Text, Triple)]
queryAllTriples root = B.toList (root ^. #subjects)

printTriples :: App ()
printTriples = do
    triples <- transactReadOnly queryAllTriples
    liftIO $ print triples

main :: IO ()
main = do
    let db = "/tmp/mtl-example.haskey"
    putStrLn $ "Using " ++ db
    main' db

main' :: FilePath -> IO ()
main' fp = do
    db <- flip runFileStoreT defFileStoreConfig $
        openConcurrentDb hnds >>= \case
            Nothing -> createConcurrentDb hnds emptySchema
            Just db -> return db

    conn <- MySQL.connect
        MySQL.defaultConnectInfo
          { MySQL.ciUser = "root"
          , MySQL.ciPassword = "root"
          , MySQL.ciDatabase = "akeneo_pim"
          , MySQL.ciHost = "localhost"
          , MySQL.ciPort = 49172
          }

    runApp app conn db defFileStoreConfig
  where
    hnds = concurrentHandles fp

newtype App a = AppT (ReaderT MySQL.MySQLConn (HaskeyT Schema IO) a)
              deriving (Functor, Applicative, Monad, MonadIO,
                        MonadHaskey Schema, MonadReader MySQL.MySQLConn)

app :: App ()
app = do
    insertSomeTriples
    printTriples
    conn <- ask
    liftIO $ BinLog.getLastBinLogTracker conn >>= \ case
        Just tracker -> do
            es <- BinLog.decodeRowBinLogEvent =<< BinLog.dumpBinLog conn 1024 tracker False
            forever $ do
                Streams.read es >>= \ case
                    Just (BinLog.RowWriteEvent _timestamp  _tracker table events)  ->
                        insertEvents []  -- events
                    Nothing -> return ()
        Nothing -> error "can't get latest binlog position"


insertEvents :: [BinLog.WriteRowsEvent] -> App ()
insertEvents events = transact_ $ \schema -> do
    foldlM insertTriple schema $ map e2t events
    commit_ schema
    where
        e2t (BinLog.WriteRowsEvent {writeRowData = d}) = Triple "a" "b" "c"

runApp :: App a
       -> MySQL.MySQLConn
       -> ConcurrentDb Schema
       -> FileStoreConfig
       -> IO a
runApp (AppT m) r = runHaskeyT (runReaderT m r)

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
  shrink = fmap pack . shrink . unpack

