{ mkDerivation, ansi-wl-pprint, base, hedgehog, lens, parsec
, parsers, pretty, semigroups, stdenv, tasty, tasty-hedgehog
, tasty-hspec, tasty-hunit, text
}:
mkDerivation {
  pname = "alphachar";
  version = "0.0.3";
  src = ./.;
  libraryHaskellDepends = [ base lens parsers semigroups ];
  testHaskellDepends = [
    ansi-wl-pprint base hedgehog lens parsec parsers pretty tasty
    tasty-hedgehog tasty-hspec tasty-hunit text
  ];
  homepage = "https://github.com/qfpl/alphachar";
  description = "A character between a-z";
  license = stdenv.lib.licenses.bsd3;
}
