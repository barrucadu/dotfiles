{ pkgs ? (import <nixpkgs> {}) }:

{
  python2Env = pkgs.stdenv.mkDerivation {
    name = "python2-env";
    buildInputs = with pkgs; [
      python2Packages.flake8
      python2Packages.virtualenv
    ];
  };
}
