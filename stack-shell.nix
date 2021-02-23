{sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs {}, ghc ? pkgs.haskell.compiler.ghc8103}:
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "cign-gtk-env";
  buildInputs = import ./common-deps.nix { inherit sources; };
}
