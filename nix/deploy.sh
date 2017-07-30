NIXPKGS="$(nix-build --no-out-link nix/nixpkgs.nix)"
NIX_PATH="nixpkgs=${NIXPKGS}" nixops deploy --deployment suns2 --force-reboot
NIX_PATH="nixpkgs=${NIXPKGS}" nixops deploy --deployment suns2
