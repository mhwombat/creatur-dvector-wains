{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cereal, containers, creatur
      , creatur-wains, creatur-wains-test-utils, gray-extended, lens
      , MonadRandom, mtl, numeric-tools, QuickCheck, som, split, stdenv
      , test-framework, test-framework-quickcheck2
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
