let
  nixpkgs-src = builtins.fetchTarball {
    # cherry-pick of https://github.com/NixOS/nixpkgs/pull/224542
    # on top of merge of https://github.com/NixOS/nixpkgs/pull/237746
    url = "https://github.com/qrlex/nixpkgs/archive/301a63455028ce683abb2663f36a0fa1464e2e13.tar.gz";
    sha256 = "sha256:14kxgssgy7pa92wnj2vhsv5jy0qf4kdwnsrsw8272x76531525cw";
  };

  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib;
      let
        # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-1371565125
        # https://github.com/NixOS/nixpkgs/issues/220647
        fixCyclicReference = drv: overrideCabal drv (_: { enableSeparateBinOutput = false; });
      in {
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            ghc96 = pkgs.haskell.packages.ghc96.override(old: {
              overrides = pkgs.lib.fold pkgs.lib.composeExtensions (old.overrides or (_: _: {})) [

                (self: super: {
                  nightfall = self.callCabal2nix "nightfall" ../. {};
                })

                # ghcid overrides
                (self: super: {
                  # some tests are non-reproducible from measuring time
                  ghcid = fixCyclicReference(dontCheck super.ghcid);

                  # bump for https://github.com/gregwebs/Shelly.hs/pull/216
                  # disable tests as they fail when sandboxing is active
                  shelly = dontCheck (self.callHackage "shelly" "1.12.1" {});
                })

                # hoogle overrides
                (self: super: {
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
                })
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
