{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, lib, ... }: {

        haskellProjects.default = {
          projectRoot = ./personal-page;
          devShell = {
            tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          };
        };

        packages."personal-page" = pkgs.stdenv.mkDerivation {
          name = "personal-page";
          buildInputs = [];
          src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;

          buildPhase = ''
            ${lib.getExe' self'.packages.compiler "compiler"} build --verbose
          '';

          installPhase = ''
            mkdir -p "$out"
            cp -r ./_site "$out"
          '';
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
