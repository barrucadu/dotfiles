{ pkgs }:
{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {
    ghc802Packages = super.haskell.packages.ghc802;

    proselint = import ./pkgs/proselint.nix { pkgs = self; };
  };
}
