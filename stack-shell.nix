{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs {}, ghc ? pkgs.haskell.compiler.ghc8103 }:
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "cign-gtk-env";
  nativeBuildInputs = import ./common-deps.nix { inherit sources; };
}
