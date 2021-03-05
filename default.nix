{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskell-nix {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
  # import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
, gitignoreSource ? (import sources.gitignore-nix {}).gitignoreSource
}: pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "cign-gtk";
    src = gitignoreSource ./.;
  };
}
