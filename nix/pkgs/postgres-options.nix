{ mkDerivation, base, bytestring, generic-monoid, hspec, split
, stdenv, uri-bytestring
}:
mkDerivation {
  pname = "postgres-options";
  version = "0.2.0.0";
  sha256 = "c45f459abcc797d0707c1931301434037acdfada33e47c843f219cf35609d9b5";
  libraryHaskellDepends = [
    base bytestring generic-monoid split uri-bytestring
  ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/jfischoff/postgres-options#readme";
  description = "An Options type representing options for postgres connections";
  license = stdenv.lib.licenses.bsd3;
}
