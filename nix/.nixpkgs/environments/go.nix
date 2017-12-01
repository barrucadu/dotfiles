{ pkgs ? (import <nixpkgs> {}) }:

{
  goEnv = pkgs.stdenv.mkDerivation {
    name = "go-env";
    buildInputs = with pkgs; [
      go
    ];
  };
}
