let
  sources = import nix/sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {};
  ghc-olay = self: super: {
    haskell =  {
      compiler.ghc8104 = pkgs-unstable.haskell.compiler.ghc8104;
    } // super.haskell;
  };
  vanilla = import sources.nixpkgs {};
  pkgs = import sources.nixpkgs { overlays = [ ghc-olay ]; };
  common-deps = pkgs.callPackage (import ./common-deps.nix) {};
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ stack nix ] ++ common-deps ++ (
    with pkgs.haskellPackages;
    [ haskell-language-server cabal-install ]
  );
  NIX_CFLAGS_COMPILE="";
  NIX_PATH = "nixpkgs=${pkgs-unstable.path}";
}
