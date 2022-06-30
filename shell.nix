  { pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      pkgs.elmPackages.elm-live
      pkgs.elmPackages.elm-test
      pkgs.nodejs-14_x
    ];
  }