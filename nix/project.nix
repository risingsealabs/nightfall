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

  shell = defaults.haskell.shell nixpkgs "ghc96" (p: [p.nightfall]) (with nixpkgs; [
    zlib
  ]);

in {
  inherit nixpkgs shell;
  inherit (nixpkgs.haskell.packages.ghc96) nightfall;
}
