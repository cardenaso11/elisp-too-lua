{ pkgsPath ? <nixpkgs> }:

{
  pkgs = import pkgsPath {
    overlays =
      let projectsOverlay =
        self: super: {
          haskellPackages = super.haskellPackages.extend (
            super.haskell.lib.packageSourceOverrides {
              elisp-too-lua = ./.;
            }
          );
        };

      in [ projectsOverlay ];
  };
}
