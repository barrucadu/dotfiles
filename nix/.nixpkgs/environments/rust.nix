{ pkgs ? (import <nixpkgs> {}) }:

{
  rustEnv = pkgs.stdenv.mkDerivation {
    name = "rust-env";
    buildInputs = with pkgs; [
      cargo
      rustc
      rustfmt
    ];
  };
}
