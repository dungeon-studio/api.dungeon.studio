{ mkDerivation, aeson, base, bytestring, collection-json
, containers, data-default, directory, either, envy, exceptions
, extra, filepath, hasbolt, http-api-data, http-conduit, http-media
, http-types, jose, lens, MissingH, mtl, network-uri, resource-pool
, retry, servant, servant-server, stdenv, text, time, transformers
, unordered-containers, uuid, wai, wai-logger, warp, yaml
}:
mkDerivation {
  pname = "dungeon-studio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring collection-json containers data-default
    directory either envy exceptions extra filepath hasbolt
    http-api-data http-conduit http-media http-types jose lens MissingH
    mtl network-uri resource-pool retry servant servant-server text
    time transformers unordered-containers uuid wai wai-logger warp
    yaml
  ];
  homepage = "https://github.com/alunduil/dungeon.studio";
  description = "Game Master's Companion";
  license = stdenv.lib.licenses.mit;
}
