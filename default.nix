{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    cabal-install 
    emit
    emit-ast
    fourmolu
    haskell-language-server
    hlint;
}

