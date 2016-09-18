{ mkDerivation, base, containers, directory, parsec, parsec-utils
, stdenv
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
  license = stdenv.lib.licenses.unfree;
}
