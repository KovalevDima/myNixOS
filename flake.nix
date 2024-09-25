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
    swww.url = "github:LGFae/swww";

    nix-colors.url = "github:misterio77/nix-colors";
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
          (import ./system/hardware.nix)
          (import ./system.nix)
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
          
              users.dmitry = {
                imports = [
                  inputs.nix-colors.homeManagerModules.default
                  ./home/display.nix
                  ./home/communication.nix
                  ./home/editor.nix
                  ./home/cli-tools.nix
                ];

                module.communication.enable = true;
                module.cli-tools.enable = true;
                module.display.enable = true;
                module.editor.enable = true;

                programs.home-manager.enable = true;
          
                colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
          
                home = {
                  homeDirectory = "/home/dmitry";
                  stateVersion = "24.05";
                };
              };
            };
          }
        ];
      };
    };
  };
}
