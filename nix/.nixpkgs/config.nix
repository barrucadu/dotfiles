{ pkgs }:
{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {
    # Development
    ghc802Packages = super.haskell.packages.ghc802;
    llvm5Debug = super.llvm_5.override { debugVersion = true; };

    profiledHaskellPackages = self.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };

    # Writing
    proselint = import ./pkgs/proselint.nix { pkgs = self; };
    vale = import ./pkgs/vale.nix { pkgs = self; };
  };
}
