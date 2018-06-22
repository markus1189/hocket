{ mkDerivation, aeson, async, attoparsec, base, brick, bytestring
, case-insensitive, ConfigFile, containers, data-default, dhall
, formatting, http-client, http-client-tls, http-types, lens
, lens-action, lens-aeson, microlens, mtl, network-uri, old-locale
, process, sorted-list, split, stdenv, tasty, tasty-hspec
, tasty-hunit, tasty-quickcheck, text, time, transformers
, unordered-containers, vector, vty, wreq
}:
mkDerivation {
  pname = "hocket";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base brick bytestring ConfigFile containers
    data-default dhall formatting http-client http-client-tls
    http-types lens lens-action lens-aeson microlens mtl network-uri
    old-locale process sorted-list split text time transformers
    unordered-containers vector vty wreq
  ];
  executableHaskellDepends = [
    async base brick case-insensitive containers data-default dhall
    formatting http-client lens lens-action lens-aeson microlens
    network-uri process text time transformers vty
  ];
  testHaskellDepends = [
    base containers lens tasty tasty-hspec tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
