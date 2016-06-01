{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  modHaskellPackages = pkgs.haskellPackages.override {
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

  myHaskellPackages = if compiler == "default"
                      then modHaskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = myHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
