{ sources ? import ./nix/sources.nix
, system ? builtins.currentSystem
, haskellNix ? import sources.haskell-nix {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
  # import nixpkgs with overlays
, pkgs-overlaid ? import nixpkgsSrc (nixpkgsArgs // { inherit system; })
, gitignoreSource ? (import sources.gitignore-nix {}).gitignoreSource
}: (pkgs-overlaid.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs-overlaid.haskell-nix.haskellLib.cleanGit {
    name = "cign-gtk";
    src = gitignoreSource ./.;
  };
}).cign-gtk.components.exes.cign-gtk
