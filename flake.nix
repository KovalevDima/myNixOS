{
  description = "My nixos configuration";

  inputs = {

    master.url = "github:NixOS/nixpkgs/master";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    stable.url = "github:NixOS/nixpkgs/nixos-24.05";

    nixpkgs.follows = "unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
  };

  outputs = { nixpkgs, self, ... } @ inputs:
  let
    username = "dmitry";
  in
  {
    nixosConfigurations = {

      nixos = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs username; };
        modules = [
          (import ./hardware.nix)
          (import ./home.nix)
          (import ./system.nix)
          inputs.home-manager.nixosModules.home-manager
        ];
      };

    };
  };
}
