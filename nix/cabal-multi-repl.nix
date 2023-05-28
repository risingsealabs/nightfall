let
  nixpkgs-src = builtins.fetchTarball {
    # https://github.com/haskell/cabal/pull/8726 on top of haskell-updates 23/03/23
    url = "https://github.com/qrlex/nixpkgs/archive/c149517ad5d6761cf22515e1f84baabbbe1fb77b.tar.gz";
    sha256 = "sha256:0jrzj4fkxnwhc3c6azq9178pn8nq3ns37y4k1v1pdm7hvrhrimkq";
  };

  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc96 = pkgs.haskell.packages.ghc96.override(old: {
            overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
              # newer ghc and/or cabal changes the value of `srcLocPackage` in
              # https://github.com/sol/call-stack/blob/1d78c6320f986a2948b2076736b1678e53f862cf/test/Data/CallStackSpec.hs#L16
              call-stack = dontCheck super.call-stack;
            });
          });
        };
      };
    };
  };

  nixpkgs = import nixpkgs-src {inherit config; };

in {
  inherit (nixpkgs.haskell.packages.ghc96) cabal-install;
}
