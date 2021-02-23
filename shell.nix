let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    gobject-introspection
    gtk3
    harfbuzz
    pango
    pkg-config
    stack
    zlib
  ] ++ (
    with pkgs.haskellPackages;
    [ haskell-language-server cabal-install ]
  );
}
