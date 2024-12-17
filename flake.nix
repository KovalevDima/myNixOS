{
  description = "My nixos configuration";

  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs.follows = "unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix.url = "github:Mic92/sops-nix";
    nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = { nixpkgs, self, ... } @ inputs: 
  let
    systemModules = [
      inputs.sops-nix.nixosModules.sops
      ./system/hyprland.nix
      ./system/gaming.nix
      ./system/i18n.nix
      ./system/unfreeSoftware.nix
      ./system/nix.nix
      ./system/k8s-dev.nix
    ];
  in {
    nixosConfigurations = {
      desktop = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/dmitry-desktop {inherit inputs systemModules;});
      laptop = inputs.nixpkgs.lib.nixosSystem
        (import ./systems/dmitry-laptop {inherit inputs systemModules;});
    };
  };
}
