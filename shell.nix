let common = import ./common.nix{};
    pkgs = common.pkgs;
in with pkgs; haskellPackages.shellFor {
  packages = p: [ p.elisp-too-lua ];
  withHoogle = true;
}