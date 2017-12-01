{ pkgs }:
with pkgs; buildGoPackage rec {
  name = "vale-unstable-${version}";
  version = "2017-09-06";
  owner = "ValeLint";
  repo = "vale";
  rev = "20ed879678910a8f68f4ace41feb45976893db2c";

  goPackagePath = "github.com/${owner}/${repo}";

  src = fetchFromGitHub {
    inherit owner repo rev;
    sha256 = "1xaj0y6v7sv47ihaz6zs5y7dxasiingma6akr1wggzb3h457nj20";
  };

  postInstall = ''
    mkdir -p $bin/share/vale/
    mv go/src/github.com/ValeLint/vale/styles $bin/share/vale/
  '';

  # For some reason this needs to be a separate file.  SAD!
  goDeps = ./vale-deps.nix;

  meta = {
    description = "A customizable, syntax-aware linter for prose";
    homepage = "https://valelint.github.io/docs/";
    license = stdenv.lib.licenses.mit;
  };
}
