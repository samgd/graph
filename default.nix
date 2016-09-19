{ mkDerivation, base, containers, directory, doctest, parsec
, parsec-utils, QuickCheck, stdenv
}:
mkDerivation {
  pname = "graph";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory parsec parsec-utils
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest QuickCheck ];
  license = stdenv.lib.licenses.unfree;
}
