{ mkDerivation, aeson, base, bytestring, connection, containers
, data-default, envy, exceptions, hasbolt, hspec, hspec-discover
, http-api-data, http-conduit, http-media, http-types, ieee754
, jose, lens, mtl, network-arbitrary, network-uri, network-uri-json
, QuickCheck, quickcheck-instances, regex-compat, resource-pool
, retry, servant, servant-server, siren-json, stdenv
, test-invariant, text, time, unordered-containers, uuid
, validation, wai, wai-cors, wai-logger, warp
}:
mkDerivation {
  pname = "api-dungeon-studio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring connection containers data-default envy
    exceptions hasbolt http-api-data http-conduit http-media http-types
    jose lens mtl network-uri network-uri-json regex-compat
    resource-pool retry servant servant-server siren-json text time
    unordered-containers uuid validation wai wai-cors wai-logger warp
  ];
  testHaskellDepends = [
    aeson base envy exceptions hspec http-api-data http-media
    http-types ieee754 jose lens network-arbitrary network-uri
    network-uri-json QuickCheck quickcheck-instances servant
    servant-server siren-json test-invariant text time
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/api.dungeon.studio";
  description = "Game Master's Companion";
  license = stdenv.lib.licenses.mit;
}
