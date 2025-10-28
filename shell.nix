let
  # XXX: Use LTS stackage 9.10.2?
  # Commit that adds GHC 9.12.2
  # nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/7a2f616d2f1111a0c7d852b0b7b9fabeb4f68de7";
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  packages = with pkgs; [
    haskell.compiler.ghc947
    cabal-install
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
  ];
}
