{ mkDerivation, aeson, base, bytestring, case-insensitive
, connection, containers, data-default, either, envy, exceptions
, extra, hasbolt, hspec, http-api-data, http-conduit, http-media
, http-types, ieee754, jose, lens, MissingH, mtl, network-arbitrary
, network-uri, network-uri-json, process, QuickCheck
, quickcheck-instances, regex-compat, resource-pool, retry, servant
, servant-server, siren-json, stdenv, test-invariant, text, time
, transformers, unordered-containers, uuid, wai, wai-logger, warp
}:
mkDerivation {
  pname = "api-dungeon-studio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring connection containers data-default either
    envy exceptions hasbolt http-api-data http-conduit http-media
    http-types jose lens MissingH mtl network-uri network-uri-json
    regex-compat resource-pool retry servant servant-server siren-json
    text time transformers unordered-containers uuid wai wai-logger
    warp
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive envy exceptions extra hspec
    http-api-data http-conduit http-media http-types ieee754 jose lens
    mtl network-arbitrary network-uri network-uri-json process
    QuickCheck quickcheck-instances regex-compat servant servant-server
    siren-json test-invariant text time unordered-containers wai
  ];
  homepage = "https://github.com/alunduil/api.dungeon.studio";
  description = "Game Master's Companion";
  license = stdenv.lib.licenses.mit;
}
