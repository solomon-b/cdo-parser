{
  description = "Rainwater Collector";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    easy-hls = {
      url = github:jkachmar/easy-hls-nix;
      inputs.nixpkgs.follows = "nixpkgs";
   };
  };

  outputs = { self, nixpkgs, easy-hls, flake-utils }:
    let
      overlay = import ./overlay.nix;
      overlays = [ overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
          haskell-language-server = pkgs.callPackage easy-hls {};
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.rainwater-collector ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            haskell-language-server  
            pkgs.postgresql
            pkgs.zlib
          ];
        };
        defaultPackage = pkgs.haskellPackages.rainwater-collector;
      }) // { inherit overlay overlays; };
}
