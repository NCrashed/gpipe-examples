{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc982;
in
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc982
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    pkgs.glfw
    pkgs.xorg.libXcursor
    pkgs.xorg.libXrandr
    pkgs.xorg.libXi
    pkgs.xorg.libX11
    pkgs.xorg.libXxf86vm
    pkgs.xorg.libXinerama
  ];
}
