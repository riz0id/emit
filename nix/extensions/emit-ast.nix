{ ghc ? "ghc922" }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        emit-ast = self.callCabal2nix "emit-ast" ../../examples/emit-ast/. { };
      });
    };
  };
}