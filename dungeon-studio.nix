{ mkDerivation, aeson, base, bytestring, containers, data-default
, either, envy, exceptions, hasbolt, http-api-data, http-media
, http-types, MissingH, mtl, network-uri, QuickCheck
, quickcheck-instances, resource-pool, retry, servant
, servant-server, stdenv, test-invariant, text, time
, unordered-containers, uuid, wai-logger, warp, yaml
}:
mkDerivation {
  pname = "dungeon-studio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers data-default either envy
    exceptions hasbolt http-api-data http-media http-types MissingH mtl
    network-uri resource-pool retry servant servant-server text time
    unordered-containers uuid wai-logger warp yaml
  ];
  testHaskellDepends = [
    aeson base bytestring containers http-api-data http-media
    http-types MissingH network-uri QuickCheck quickcheck-instances
    test-invariant text unordered-containers
  ];
  homepage = "https://github.com/alunduil/dungeon.studio";
  description = "Game Master's Companion";
  license = stdenv.lib.licenses.mit;
}
