let
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/0d1b9472176bb31fa1f9a7b86ccbb20c656e6792.tar.gz"; # haskell-updates 23/03/23
    sha256 = "0j7y0s691xjs2146pkssz5wd3dc5qkvzx106m911anvzd08dbx9f";
  };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.extend (self: super: with pkgs.haskell.lib; {
        nightfall = self.callCabal2nix "nightfall" ../. {};
      });
    };
  };

  nixpkgs = import nixpkgsSrc { inherit config; };

  shell = nixpkgs.haskellPackages.shellFor {
    strictDeps = true;
    packages = p: [ p.nightfall ];
    withHoogle = true;
    nativeBuildInputs =
      let hask = with nixpkgs.haskellPackages; [
        cabal-install
        ghcid
        (haskell-language-server.overrideAttrs(finalAttrs: previousAttrs: { propagatedBuildInputs = []; buildInputs = previousAttrs.propagatedBuildInputs; }))
      ];
      in hask ++ [
        nixpkgs.zlib
      ];
  };
in

{ inherit nixpkgs shell; }
