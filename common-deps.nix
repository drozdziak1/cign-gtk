{ sources ? import nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, cign ? import sources.cign { nixpkgs = pkgs; }
}:
with pkgs; [
  cign
  gobject-introspection
  gtk3
  # harfbuzz
  # pango
  pkg-config
  zlib
]
