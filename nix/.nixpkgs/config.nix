{ pkgs }:
{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {
    ghc802Packages = super.haskell.packages.ghc802;
  };
}
