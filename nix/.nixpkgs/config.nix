{ pkgs }:
{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {
    proselint = import ./pkgs/proselint.nix { pkgs = self; };
    vale = import ./pkgs/vale.nix { pkgs = self; };
  };
}
