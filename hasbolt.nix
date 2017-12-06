{ mkDerivation, base, binary, bytestring, connection, containers
, data-binary-ieee754, data-default, fetchgit, hex, hspec, network
, QuickCheck, stdenv, text, transformers
}:
mkDerivation {
  pname = "hasbolt";
  version = "0.1.3.0";
  src = fetchgit {
    url = "https://github.com/zmactep/hasbolt";
    sha256 = "1l1bgjwgaig72b6lim2178a44p751zsnac73wrrspb70ckz1v9vx";
    rev = "1aa8b55a0b366192885f92d40d61b85947fdc5af";
  };
  libraryHaskellDepends = [
    base binary bytestring connection containers data-binary-ieee754
    data-default hex network text transformers
  ];
  testHaskellDepends = [
    base bytestring containers hex hspec QuickCheck text
  ];
  homepage = "https://github.com/zmactep/hasbolt#readme";
  description = "Haskell driver for Neo4j 3+ (BOLT protocol)";
  license = stdenv.lib.licenses.bsd3;
}
