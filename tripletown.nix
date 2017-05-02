{ mkDerivation, base, containers, hspec, mtl, random, stdenv }:
mkDerivation {
  pname = "tripletown";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers mtl random ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers hspec mtl ];
  homepage = "https://github.com/tripletown/dmjio";
  description = "A console-based version of http://spryfox.com/our-games/tripletown/";
  license = stdenv.lib.licenses.bsd3;
}
