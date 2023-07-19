let
  miden = nixpkgs: nixpkgs.rustPlatform.buildRustPackage rec {
    pname = "miden";
    version = "0.5.0";
    src = nixpkgs.fetchFromGitHub {
      owner = "0xPolygonMiden";
      repo = "miden-vm";
      rev = "v${version}";
      sha256 = "sha256-bDILZ8vvxjB/JN6tyhTNKxjQB/35M47grxFqToesH9E=";
    };
    cargoLock.lockFile = ./miden_cargo.lock;
    postPatch = ''
      ln -s ${./miden_cargo.lock} Cargo.lock
    '';
    buildType = "release";
    buildFeatures = [ "executable" "concurrent" ];
    nativeBuildInputs = with nixpkgs; [ rustc ];
    doCheck = false;
  };
in

(import ./defaults.nix).projects.haskell ({nixpkgs, ...}: {
  compiler = "ghc96";

  haskellOverrides = self: super: {};

  projectPackages = {
    nightfall = ../.;
  };

  # shellTools = with nixpkgs; [];
  shellTools = [ (miden nixpkgs) ];
})
