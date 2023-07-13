let
  inherit (import ./pkgs.nix) overrides nixpkgs-src;

  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc96 = pkgs.haskell.packages.ghc96.override(old: {
            overrides = pkgs.lib.fold pkgs.lib.composeExtensions (old.overrides or (_: _: {})) [
              (self: super: { nightfall = self.callCabal2nix "nightfall" ../. {}; })
              (overrides pkgs).ghcid
              (overrides pkgs).hoogle
              (overrides pkgs).pairing
            ];
          });
        };
      };
    };
  };

  nixpkgs = import nixpkgs-src { inherit config; };

  shell = nixpkgs.haskell.packages.ghc96.shellFor {
    strictDeps = true;
    packages = p: [ p.nightfall ];
    withHoogle = true;
    nativeBuildInputs =
      let hask = with nixpkgs.haskell.packages.ghc96; [
        (import ./cabal-multi-repl.nix).cabal-install # Shouldn't be needed once this cabal is bundled with the compiler, likely ghc 9.8 / Cabal 3.12
        ghcid
        (haskell-language-server.overrideAttrs(finalAttrs: previousAttrs: { propagatedBuildInputs = []; buildInputs = previousAttrs.propagatedBuildInputs; }))
      ];
      in with nixpkgs; hask ++ [
        zlib
      ];
  };
in {
  inherit nixpkgs shell;
  inherit (nixpkgs.haskell.packages.ghc96) nightfall;
}
