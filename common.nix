{
  pkgsPath ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/3fa154fd7fed3d6a94322bf08a6def47d6f8e0f6.tar.gz";

  }
}:

{
  pkgs = import pkgsPath {
    overlays =
      let projectsOverlay =
        self: super: {
          haskellPackages = super.haskell.packages.ghc864.extend (
            super.haskell.lib.packageSourceOverrides {
              elisp-too-lua = ./.;
            }
          );
        };
          dontTestOverlay = 
            self: super: {
              haskellPackages = super.haskellPackages.extend (innerSelf: innerSuper:
              {
                generic-lens = super.haskell.lib.dontCheck innerSuper.generic-lens;
              });
            };
        

      in [ projectsOverlay dontTestOverlay ];
  };
}
