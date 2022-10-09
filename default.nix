{ ghc ? "ghc902" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    emit
    fourmolu
    haskell-language-server
    hlint;
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;

}

