{
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs.follows = "unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix.url = "github:Mic92/sops-nix";
    nix-colors.url = "github:misterio77/nix-colors";
    nix-bitcoin.url = "github:fort-nix/nix-bitcoin";

    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = { self, nixpkgs, flake-parts, disko, ... } @ inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, lib, config, ... }: {

        haskellProjects.default = {
          autoWire = [ "packages" ];
          projectRoot = ./.;
          settings = {
            graphics = {
              justStaticExecutables = true;
              extraBuildDepends = [ pkgs.glslang ];
            };
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          packages = with pkgs; [ glslang ];
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
      };
  }
  //
  {
    nixosConfigurations = {
      desktop = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/desktop
          { inherit inputs;
            systemModules = [
              inputs.sops-nix.nixosModules.sops
              ./modules/system/hyprland.nix
              ./modules/system/i18n.nix
              ./modules/system/nix.nix
              ./modules/system/k8s-dev.nix
            ];
            homeModules = [
              inputs.nix-colors.homeManagerModules.default
              ./modules/home/tui.nix
              ./modules/home/gui.nix
            ];
          }
        );
      laptop = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/laptop
          { inherit inputs;
            systemModules = [
              inputs.sops-nix.nixosModules.sops
              ./modules/system/hyprland.nix
              ./modules/system/i18n.nix
              ./modules/system/nix.nix
              ./modules/system/k8s-dev.nix
            ];
            homeModules = [
              inputs.nix-colors.homeManagerModules.default
              ./modules/home/tui.nix
              ./modules/home/gui.nix
            ];
          }
        );
      homeserver = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/homeserver
          { inherit inputs disko;
            systemModules = [
              inputs.sops-nix.nixosModules.sops
              inputs.nix-bitcoin.nixosModules.default
              ./modules/system/i18n.nix
              ./modules/system/nix.nix
            ];
            homeModules = [
              inputs.nix-colors.homeManagerModules.default
              ./modules/home/tui.nix
            ];
          }
        );
      };
    };
}
