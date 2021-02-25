let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  common-deps = pkgs.callPackage (import ./common-deps.nix) {};
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ stack stack2nix ] ++ common-deps ++ (
    with pkgs.haskellPackages;
    [ haskell-language-server cabal-install ]
  );
}
