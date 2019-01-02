{ mkDerivation, base, cereal, containers, creatur, creatur-wains
, creatur-wains-test-utils, gray-extended, lens, MonadRandom, mtl
, numeric-tools, QuickCheck, som, split, stdenv, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "creatur-dvector-wains";
  version = "1.1.2";
  src = ./.;
  libraryHaskellDepends = [
    base cereal containers creatur creatur-wains gray-extended lens
    MonadRandom mtl som split
  ];
  testHaskellDepends = [
    base creatur-wains creatur-wains-test-utils numeric-tools
    QuickCheck test-framework test-framework-quickcheck2
  ];
  homepage = "https://github.com/mhwombat/creatur-dvector-wains#readme";
  license = stdenv.lib.licenses.bsd3;
}
