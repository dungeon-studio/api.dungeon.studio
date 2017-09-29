{ mkDerivation, aeson, base, bytestring, containers, data-default
, directory, envy, exceptions, extra, filepath, hasbolt
, http-api-data, http-media, http-types, MissingH, mtl, network-uri
, resource-pool, retry, servant, servant-server, stdenv, text, time
, unordered-containers, uuid, wai-logger, warp, yaml
}:
mkDerivation {
  pname = "dungeon-studio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers data-default directory envy
    exceptions extra filepath hasbolt http-api-data http-media
    http-types MissingH mtl network-uri resource-pool retry servant
    servant-server text time unordered-containers uuid wai-logger warp
    yaml
  ];
  homepage = "https://github.com/alunduil/dungeon.studio";
  description = "Game Master's Companion";
  license = stdenv.lib.licenses.mit;
}
