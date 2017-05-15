{ pkgs }:
with pkgs; buildGoPackage rec {
  name = "vale-unstable-${version}";
  version = "2017-05-14";
  owner = "ValeLint";
  repo = "vale";
  rev = "215834049dc696ba6e1dc76ec14402c91d03b78e";

  goPackagePath = "github.com/${owner}/${repo}";

  src = fetchFromGitHub {
    inherit owner repo rev;
    sha256 = "187x6fnsy458g4my9lh4y5wzd1rg5gnin46icb5m3190j5lakpq0";
  };

  # For some reason this needs to be a separate file.  SAD!
  goDeps = ./vale-deps.nix;

  meta = {
    description = "A customizable, syntax-aware linter for prose";
    homepage = "https://valelint.github.io/docs/";
    license = stdenv.lib.licenses.mit;
  };
}
