{
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs.follows = "unstable";

    nixpkgs-old.url = "github:NixOS/nixpkgs/nixos-22.11";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-colors.url = "github:misterio77/nix-colors";
    nix-bitcoin.url = "github:fort-nix/nix-bitcoin";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = {self, flake-parts, nixpkgs, disko, ...} @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [];
      perSystem = {self', pkgs, config, lib, ...}:
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              perf
            ];
            inputsFrom = [];
          };
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
