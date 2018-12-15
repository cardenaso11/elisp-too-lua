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
          dontTestOverlay = 
            self: super: {
              haskellPackages = super.haskellPackages.extend (innerSelf: innerSuper:
                { generic-lens = super.haskell.lib.dontCheck innerSuper.generic-lens; }
                );
            };
        

      in [ projectsOverlay dontTestOverlay ];
  };
}
