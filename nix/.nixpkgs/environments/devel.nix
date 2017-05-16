{ pkgs ? (import <nixpkgs> {}) }:

{
  develEnv = pkgs.stdenv.mkDerivation {
    name = "devel-env";
    buildInputs = with pkgs; [
      bison
      clang
      gcc
      gdb
      gnumake
      m4
      pkgconfig
      valgrind
    ];
  };
}
