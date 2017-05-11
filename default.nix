{  pkgs ? import <nixpkgs> {} }:
with pkgs; with pkgs.haskellPackages;
mkDerivation {
  pname = "clustertest";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ cabal-install stack ];
  libraryHaskellDepends = [ haskellPackages.base ];
  executableHaskellDepends = [ haskellPackages.base haskellPackages.distributed-process haskellPackages.distributed-process-simplelocalnet];
  testHaskellDepends = [ haskellPackages.base ];
  description = "A basic compute node.";
  license = stdenv.lib.licenses.bsd3.shortName;
}
