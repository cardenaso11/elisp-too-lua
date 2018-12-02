{ pkgsPath ? <nixpkgs> }:

let common = import ./common.nix { inherit pkgsPath; };
in with common.pkgs; {
  main-build = haskellPackages.elisp-too-lua;
  extras = {};
}