{ mkDerivation, aeson, base, bifunctors, containers, deepseq
, hashable, HUnit, lens, QuickCheck, quickcheck-text, scientific
, servant-docs, stdenv, swagger2, tasty, tasty-hunit
, tasty-quickcheck, text, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-injector";
  version = "1.1.5.0";
  sha256 = "a467f51dc392b1eaeaca1234fae9b042afdcbab2e87520bd51b8ee05e128e841";
  libraryHaskellDepends = [
    aeson base bifunctors deepseq hashable lens servant-docs swagger2
    text unordered-containers
  ];
  testHaskellDepends = [
    aeson base containers HUnit lens QuickCheck quickcheck-text
    scientific swagger2 tasty tasty-hunit tasty-quickcheck text vector
  ];
  description = "Injecting fields into aeson values";
  license = stdenv.lib.licenses.mit;
}
