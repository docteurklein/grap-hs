{ compiler ? "ghc8104" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { config.allowBroken = true; };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: with pkgs.haskell.lib; {

      haskey = doJailbreak (dontCheck hsuper.haskey);
      generic-lens-labels = doJailbreak (dontCheck hsuper.generic-lens-labels);

      "graphs" =
        hself.callCabal2nix
          "graphs"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."graphs"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."graphs");

  docker = pkgs.dockerTools.buildImage {
    name = "graphs";
    config.Cmd = [ "${exe}/bin/graphs" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "graphs" = myHaskellPackages."graphs";
}
