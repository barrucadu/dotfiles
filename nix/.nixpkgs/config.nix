{ pkgs }:
{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {
    ghc802Packages = super.haskell.packages.ghc802;

    proselint = with pkgs.python35Packages; buildPythonPackage rec {
      name = "proselint-${version}";
      version = "0.8.0";

      src = pkgs.fetchurl {
        url = "mirror://pypi/p/proselint/${name}.tar.gz";
        sha256 = "08d48494533f178eb7a978cbdf10ddf85ed7fc2eb486ff5e7d0aecfa08e81bbd";
      };

      buildInputs = [ future six ];
      propagatedBuildInputs = [ click ];

      doCheck = false;

      meta = {
        description = "A linter for prose";
        homepage = "http://proselint.com/";
        license = pkgs.stdenv.lib.licenses.bsd3;
      };
    };
  };
}
