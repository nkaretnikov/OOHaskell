{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

with nixpkgs.pkgs;
let
  selectedHaskellPackages = if compiler == "default"
                            then haskellPackages
                            else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = selectedHaskellPackages.override {
    overrides = self: super: {
      HList = self.callPackage ../HList {};
    };
  };

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

  drv = modifiedHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
