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
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://zero-to-flakes.com/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://zero-to-flakes.com/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   aeson.source = "1.5.0.0"; # Hackage version override
          #   shower.source = inputs.shower;
          # };
          # settings = {
          #   aeson = {
          #     check = false;
          #   };
          #   relude = {
          #     haddock = false;
          #     broken = false;
          #   };
          # };

          devShell = {
            tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          
            # hlsCheck.enable = true;
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
