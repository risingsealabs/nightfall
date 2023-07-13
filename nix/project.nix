(import ./pkgs.nix).defaults.project.haskell ({nixpkgs, ...}: {
  compiler = "ghc96";

  haskellOverrides = self: super: {};

  projectPackages = {
    nightfall = ../.;
  };

  shellTools = with nixpkgs; [];
})
