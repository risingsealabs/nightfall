let
  inherit (import ./pkgs.nix) defaults nixpkgs-src overrides;

  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = defaults.haskell.packages pkgs
          (self: super: {
            nightfall = self.callCabal2nix "nightfall" ../. {};
          })
        ;
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
