{ pkgs
, haskellSuper
, haskell
}:
haskell.lib.overrideCabal haskellSuper.cheops-email (drv: {
  enableSharedExecutables = false;
  enableSeparateDataOutput = true;
  enableSeparateDocOutput = true;
  enableLibraryProfiling = false;
  isLibrary = true;
  doHaddock = false;
  doCheck = true;
  testDepends = (drv.testDepends or []);
  testTarget = "--show-details=streaming";
})
