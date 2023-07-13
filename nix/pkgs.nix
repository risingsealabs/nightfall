let
  nixpkgs-src = builtins.fetchTarball {
    # merge of https://github.com/NixOS/nixpkgs/pull/237712 into master
    url = "https://github.com/NixOS/nixpkgs/archive/6eb5c1b5bacbb28804abbb5f4421003b34333685.tar.gz";
    sha256 = "sha256:1042nf80irf213rv4ifbxs8k2xbqqxq2nnk7nifip5zkrbk9rlq6";
  };

  overrides = pkgs: with pkgs.haskell.lib;
    let
      # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-1371565125
      # https://github.com/NixOS/nixpkgs/issues/220647
      fixCyclicReference = drv: overrideCabal drv (_: { enableSeparateBinOutput = false; });
    in {
      ghcid = self: super: {
        # some tests are non-reproducible from measuring time
        ghcid = fixCyclicReference (dontCheck super.ghcid);

        # bump for https://github.com/gregwebs/Shelly.hs/pull/216
        # disable tests as they fail when sandboxing is active
        shelly = dontCheck (self.callHackage "shelly" "1.12.1" {});
      };

      hoogle = self: super: {
        # ERROR: Network.Socket.bind: permission denied (Operation not permitted)
        http2 = dontCheck super.http2;

        # https://github.com/sjakobi/bsb-http-chunked/issues/45
        bsb-http-chunked = dontCheck super.bsb-http-chunked;

        # https://github.com/yesodweb/wai/pull/926
        # tests hang
        warp = dontCheck (self.callHackage "warp" "3.3.25" {});

        # warp bump requires these bumps
        recv = self.callHackage "recv" "0.1.0" {};
        warp-tls = self.callHackage "warp-tls" "3.3.5" {};
      };

      pairing = self: super: {
        # https://github.com/protolude/protolude/pull/143#issuecomment-1589406228
        protolude = overrideCabal (self.callHackage "protolude" "0.3.3" {}) (_: {
          revision = "1";
          editedCabalFile = "sha256-W06ZNxNaF2EdBwmwVsRHC+APa64QBq4r2zQwCwbSDh4=";
        });

        # https://github.com/serokell/galois-field/pull/2
        galois-field = doJailbreak (self.callCabal2nix "galois-field" (pkgs.fetchFromGitHub {
          owner = "serokell";
          repo = "galois-field";
          rev = "6fb4511eebbd3363baa9e02cbb51d91642d02740";
          sha256 = "sha256-vlBmOT+jzW+txBRUZsj5vfXx5f51iECxZzPvrVs2cUU=";
        }) {});

        # https://github.com/serokell/elliptic-curve/pull/1
        elliptic-curve = doJailbreak (self.callCabal2nix "elliptic-curve" (pkgs.fetchFromGitHub {
          owner = "serokell";
          repo = "elliptic-curve";
          rev = "6982573859ca72b53412ea31ba0109a051b1adf2";
          sha256 = "sha256-8zZGfdIIuUGsMvTusQA3NMKBpjyMMhkebNGTB3UPTjI=";
        }) {});

        # https://github.com/serokell/pairing/pull/1
        pairing = doJailbreak (self.callCabal2nix "pairing" (pkgs.fetchFromGitHub {
          owner = "serokell";
          repo = "pairing";
          rev = "5758deb5567c2ea90a0d4ee6e3f37fcb1e715841";
          sha256 = "sha256-W/xyVIid4rcdWa5fCxTqwyKO5YFlC1UgY+MGwHZfOK8=";
        }) {});
      };
    };

  haskellPackagesOverrides = nixpkgs: ghc: projectPackages: {
    "${ghc}" = nixpkgs.haskell.packages."${ghc}".override(old: {
      overrides = nixpkgs.lib.fold nixpkgs.lib.composeExtensions (old.overrides or (_: _: {})) [
        projectPackages
        (overrides nixpkgs).ghcid
        (overrides nixpkgs).hoogle
        (overrides nixpkgs).pairing
      ];
    });
  };

  defaults = {
    haskell = {
      packages = nixpkgs: projectPackages: with nixpkgs.haskell.lib;
        nixpkgs.haskell.packages
        // haskellPackagesOverrides nixpkgs "ghc94" projectPackages
        // haskellPackagesOverrides nixpkgs "ghc96" projectPackages;

      shell = nixpkgs: ghc: projectPkgs: tools: with nixpkgs.haskell.packages."${ghc}"; shellFor {
        strictDeps = true;
        packages = projectPkgs;
        withHoogle = true;
        nativeBuildInputs = tools ++ [
          (import ./cabal-multi-repl.nix).cabal-install # Shouldn't be needed once this cabal is bundled with the compiler, likely ghc 9.8 / Cabal 3.12
          ghcid
          (haskell-language-server.overrideAttrs(finalAttrs: previousAttrs: { propagatedBuildInputs = []; buildInputs = previousAttrs.propagatedBuildInputs; }))
        ];
      };
    };
  };

in {
  inherit nixpkgs-src overrides defaults;
}
