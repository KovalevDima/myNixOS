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
    haskell-flake.url = "github:srid/haskell-flake";
    flake-parts.url = "github:hercules-ci/flake-parts";
    services-flake.url = "github:juspay/services-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    ClickHaskell = {
      url = "github:KovalevDima/ClickHaskell";
      inputs.haskell-flake.follows = "haskell-flake";
      inputs.flake-parts.follows = "flake-parts";
      inputs.services-flake.follows = "services-flake";
      inputs.process-compose-flake.follows = "process-compose-flake";
    };
  };

  outputs = {self, flake-parts, nixpkgs, disko, ...} @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.process-compose-flake.flakeModule
      ];
      perSystem = {self', pkgs, config, lib, ...}:
      {
        process-compose = {
          default = import ./contrib/localCompose.nix {
            inherit inputs pkgs;
            executable = self'.apps.graphics;
          };
        };
        haskellProjects.default = import ./contrib/haskell.nix {inherit pkgs; ghc = "ghc984";};
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskell-language-server
              ghcid
              cabal-install
              pkgs.nodejs
              pkgs.nil
              pkgs.zlib
              pkgs.pkg-config
              pkgs.glslang
              pkgs.vulkan-tools
              pkgs.vulkan-loader
              pkgs.linuxPackages.perf
            ];
            inputsFrom = [config.haskellProjects.default.outputs.devShell];
          };
        };
        packages = {
          image = import packages/graphics/image.nix {inherit pkgs; graphics = self'.packages.graphics;};
        };
      };
  }
  //
  {
    nixosConfigurations = {
      desktop = nixpkgs.lib.nixosSystem (import ./systems/desktop
        { inherit inputs;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            ./modules/system/hyprland.nix
            ./modules/system/nix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/hyprland.nix
            ./modules/home/k8s-dev.nix
            ./modules/home/waybar.nix
            ./modules/home/alacritty.nix
            ./modules/home/nvim.nix
          ];
        }
      );
      laptop = nixpkgs.lib.nixosSystem (import ./systems/laptop
        { inherit inputs;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            ./modules/system/hyprland.nix
            ./modules/system/nix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/hyprland.nix
            ./modules/home/alacritty.nix
            ./modules/home/waybar.nix
            ./modules/home/nvim.nix
          ];
        }
      );
    };
  };
}
