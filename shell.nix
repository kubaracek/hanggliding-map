{ pkgs ? import <nixpkgs> {} }:
let
  unstable = import <nixos-unstable> {};
  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "45f5db65fc2453e757c60ae54c611d1d8baa20cf";
    sha256 = "1gc3p5xivb2k9jm22anzm6xy1cnzw2ab6jq8ifws92pvfnvx0lxv";
  }) { inherit pkgs; };
in
  with pkgs;
  mkShell {
    buildInputs = with elmTools; [ elm-test elm-verify-examples elmPackages.elm-format unstable.elmPackages.elm yarn nodejs elmPackages.elm-format elm2nix ];
    shellHook = ''
    '';
  }
