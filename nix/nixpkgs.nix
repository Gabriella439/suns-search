let
  pkgs = import <nixpkgs> {};

  inherit (pkgs) fetchFromGitHub;

  json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

in
  fetchFromGitHub {
    owner = "NixOS";

    repo = "nixpkgs";

    inherit (json) rev sha256;
  }
