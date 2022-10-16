{ ghc ? "ghc942" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.emit.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ (with pkgs; [ 
    # cabal-install
    # fourmolu
    # haskell-language-server
    # hlint
  ]);
})
