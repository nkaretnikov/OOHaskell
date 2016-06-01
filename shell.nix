{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, cabal-install, HList, stdenv }:
      mkDerivation {
        pname = "OOHaskell";
        version = "0.1.0.0";
        sha256 = "./.";
        libraryHaskellDepends = [ array base HList ];
        buildTools = [ cabal-install ];
        homepage = "http://code.haskell.org/OOHaskell/";
        description = "Haskell's overlooked object system";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
