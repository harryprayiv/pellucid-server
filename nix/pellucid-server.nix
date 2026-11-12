{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, bytestring, containers, cryptohash-sha256, directory, entropy
, filepath, http-types, lib, optparse-applicative, password
, postgresql-simple, process, resource-pool, tasty, tasty-hunit
, tasty-quickcheck, temporary, text, time, transformers, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "pellucid-server";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    containers cryptohash-sha256 directory entropy filepath http-types
    password postgresql-simple resource-pool text time transformers wai
    wai-extra
  ];
  executableHaskellDepends = [
    base optparse-applicative text time warp
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath http-types
    process tasty tasty-hunit tasty-quickcheck temporary text time wai
    wai-extra
  ];
  description = "The Pellucid server: a photo-sharing server that speaks the Pixelfed client API";
  license = lib.licenses.agpl3Plus;
  mainProgram = "pellucid-server";
}
