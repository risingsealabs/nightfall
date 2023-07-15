let
  # merge into master of https://github.com/NixOS/nixpkgs/pull/237712
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6eb5c1b5bacbb28804abbb5f4421003b34333685.tar.gz";
    sha256 = "sha256:1042nf80irf213rv4ifbxs8k2xbqqxq2nnk7nifip5zkrbk9rlq6";
  };

  overrideGroups = pkgs: with pkgs.haskell.lib;
    let
      # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-1371565125
      # https://github.com/NixOS/nixpkgs/issues/220647
      fixCyclicReference = drv: overrideCabal drv (_: { enableSeparateBinOutput = false; });

      # merge into master of https://github.com/haskell/cabal/pull/8726
      cabalSrc = pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "cabal";
        rev = "94615d6ac6ef585329eab192ed70486e722eeca1";
        sha256 = "sha256-2nSTlPMHEXtv3XsDVBCR8EdeYmSQYztQno+NQIJbf1c=";
      };

    in {
      cabal = self: super: {
        ghc = super.ghc.overrideAttrs(finalAttrs: previousAttrs: {
          prePatch = ''
            pushd Libraries
            rm -rf Cabal
            cp -r ${cabalSrc} Cabal
            rm -rf Cabal/.git
            chmod -R +w *
            popd
          '' + (previousAttrs.prePatch or "");
        });

        Cabal = self.callCabal2nix "Cabal" (cabalSrc + "/Cabal") {};
        Cabal-syntax = self.callCabal2nix "Cabal-syntax" (cabalSrc + "/Cabal-syntax") {};
        cabal-install-solver = self.callCabal2nix "cabal-install-solver" (cabalSrc + "/cabal-install-solver") {};
        cabal-install = self.callCabal2nix "cabal-install" (cabalSrc + "/cabal-install") {
          # Taken from the generated file nixpkgs/pkgs/development/haskell-modules/hackage-packages.nix
          Cabal-described = null;
          Cabal-QuickCheck = null;
          Cabal-tree-diff = null;
        };

        # newer ghc and/or cabal changes the value of `srcLocPackage` in
        # https://github.com/sol/call-stack/blob/1d78c6320f986a2948b2076736b1678e53f862cf/test/Data/CallStackSpec.hs#L16
        call-stack = dontCheck super.call-stack;
      };

      ghcid = self: super: {
        # some tests are non-reproducible from measuring time
        ghcid = fixCyclicReference (dontCheck super.ghcid);

        # bump for https://github.com/gregwebs/Shelly.hs/pull/216
        # disable tests as they fail when sandboxing is active
        shelly = dontCheck (self.callHackage "shelly" "1.12.1" {});
      };

      hlint = self: super: {
        hlint = self.callHackageDirect {
          pkg = "hlint";
          ver = "3.6";
          sha256 = "sha256-qoCzV8kTxY6zZ0hjqUa4CiLhxezxHrrSOH9u+vLSvh8=";
        } {};
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

  overrides = nixpkgs: compiler: with nixpkgs.lib;
    let
      exts = with (overrideGroups nixpkgs); [
        ghcid
        hoogle
        pairing
      ] ++ lists.optional (versionAtLeast compiler "ghc96") hlint;
    in
      fold composeExtensions (_: _: {}) exts;

  configHaskellOverrides = compiler: mkOverrides: {
    packageOverrides = nixpkgs: {
      haskell = nixpkgs.haskell // {
        packages = nixpkgs.haskell.packages // {
          "${compiler}" = nixpkgs.haskell.packages.${compiler}.override(old: {
            overrides = mkOverrides old;
          });
        };
      };
    };
  };

  shells = {
    haskell = nixpkgs: compiler: projectPkgs: tools: with nixpkgs.haskell.packages.${compiler}; shellFor {
      strictDeps = true;
      packages = projectPkgs;
      withHoogle = true;
      nativeBuildInputs =
      let
        nixpkgs-with-cabal-override = import nixpkgsSrc {
          config = configHaskellOverrides compiler (old: (overrideGroups nixpkgs).cabal);
        };
      in
        tools ++ [
          ghcid

          hlint

          (haskell-language-server.overrideAttrs(finalAttrs: previousAttrs: {
            propagatedBuildInputs = [];
            buildInputs = previousAttrs.propagatedBuildInputs;
          }))

          # Shouldn't be needed once multi-repl cabal is bundled with the compiler, (likely ghc 9.8 / Cabal 3.12)
          # ghci bug in 9.4 prevents proper use, so enable only for 9.6
          (with nixpkgs.lib.strings;
            if versionAtLeast compiler "ghc96" && versionOlder compiler "ghc98"
            then nixpkgs-with-cabal-override.haskell.packages.${compiler}.cabal-install
            else cabal-install
          )
        ];
    };
  };

  projects = {
    haskell = mkProject:
      let
        args = mkProject { inherit nixpkgs; };
        nixpkgs = mkNixpkgs { inherit config; };
        mkNixpkgs = import (args.nixpkgsSrc or nixpkgsSrc);

        compiler = args.compiler or "ghc96";
        shellTools = args.shellTools or [];
        haskellOverrides = args.haskellOverrides or (_: _: {});
        projectPackages = args.projectPackages or {};

        combineOverrides = args.combineOverrides or (overrides: with nixpkgs.lib; fold composeExtensions (_: _: {}) [
          overrides.default
          overrides.package
          overrides.project
        ]);

        config = configHaskellOverrides compiler (old: combineOverrides {
          default = nixpkgs.lib.composeExtensions old.overrides (overrides nixpkgs compiler);
          package = self: super: builtins.mapAttrs
            (n: v: self.callCabal2nix n (nixpkgs.nix-gitignore.gitignoreSource [] v) {})
            projectPackages;

          project = haskellOverrides;
        });

        shellPackages = p: nixpkgs.lib.mapAttrsToList (n: _: p.${n}) projectPackages;

      in {
        inherit nixpkgs;
        haskellPackages = nixpkgs.haskell.packages.${compiler};
        shell = shells.haskell nixpkgs compiler shellPackages shellTools;
      };
    };
in {
  inherit nixpkgsSrc overrides overrideGroups shells projects;
}
