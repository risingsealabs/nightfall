{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/549a132b51de555fed118d56c78580df889f3a12.tar.gz";
    sha256 = "1a0bs70jmrz8awdaspkhlb86i06i30qq8zfylx9i6n446diiv6fa";
  }) {}
}:
let 
  haskellPkgs = pkgs.haskell.packages.ghc961;
  nightfall = haskellPkgs.callCabal2nix "nightfall" ./nightfall.cabal {};
  ghc = haskellPkgs.ghcWithPackages (_: with nightfall.getCabalDeps; 
    (executableHaskellDepends ++ libraryHaskellDepends ++ testHaskellDepends));
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}
