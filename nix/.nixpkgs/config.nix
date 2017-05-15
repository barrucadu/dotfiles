{ pkgs }:
{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {
    # Development
    ghc802Packages = super.haskell.packages.ghc802;

    # Writing
    proselint = import ./pkgs/proselint.nix { pkgs = self; };
    vale = import ./pkgs/vale.nix { pkgs = self; };
  };
}
