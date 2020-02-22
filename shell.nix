{ pkgs ? import <nixpkgs> {} }:
let
  unstable = import <nixos-unstable> {};
in
  with pkgs;
  mkShell {
    buildInputs =  [ elmPackages.elm-format unstable.elmPackages.elm yarn nodejs elmPackages.elm-format elm2nix ];
    shellHook = ''
    '';
  }
