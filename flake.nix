{
  description = "My nixos configuration";

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
  };

  outputs = { nixpkgs, self, disko, ... } @ inputs: 
  {
    nixosConfigurations = {
      desktop = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/dmitry-desktop
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
        (import ./systems/dmitry-laptop
          { inherit inputs;
            systemModules = [
              inputs.sops-nix.nixosModules.sops
              ./modules/hyprland.nix
              ./modules/i18n.nix
              ./modules/nix.nix
              ./modules/k8s-dev.nix
            ];
            homeModules = [
              inputs.nix-colors.homeManagerModules.default
              ./modules/home/tui.nix
              ./modules/home/gui.nix
            ];
          }
        );
      homeserver = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/dmitry-homeserver
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
