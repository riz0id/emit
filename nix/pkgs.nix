{ ghc ? "ghc922" }:

let
  nixpkgs = import <nixpkgs-unstable>; 
in nixpkgs {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions [  
      (import extensions/emit.nix {
        inherit ghc;
      })
      (import extensions/emit-ast.nix {
        inherit ghc;
      }) 
      (import extensions/prim-char.nix {
        inherit ghc;
      })
      (import extensions/prim-compat.nix {
        inherit ghc;
      })
      (import extensions/prim-bool.nix {
        inherit ghc;
      })
      (import extensions/prim-int.nix {
        inherit ghc;
      })
      (import extensions/prim-ord.nix {
        inherit ghc;
      })
    ] pkgs pkgs;
}