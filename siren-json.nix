{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, fetchgit, hspec, http-media, http-types, network-uri
, network-uri-json, QuickCheck, quickcheck-instances, stdenv
, test-invariant, text, unordered-containers
}:
mkDerivation {
  pname = "siren-json";
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/alunduil/siren-json.hs";
    sha256 = "01mib3qmmdvi335asxxwhrysz4i08i8kpkf18pnzj416ic4rns1s";
    rev = "7d0880db49adac2c521a461dafd9a999afcb9af9";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers http-media http-types network-uri
    network-uri-json text unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive containers hspec http-media
    http-types network-uri network-uri-json QuickCheck
    quickcheck-instances test-invariant text unordered-containers
  ];
  homepage = "https://github.com/alunduil/siren-json.hs";
  description = "Siren Tools for Haskell";
  license = stdenv.lib.licenses.mit;
}
