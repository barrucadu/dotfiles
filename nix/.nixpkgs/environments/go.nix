{ pkgs ? (import <nixpkgs> {}) }:

{
  goEnv = pkgs.stdenv.mkDerivation {
    name = "go-env";
    buildInputs = with pkgs; [
      go_1_7
    ];
  };
}
