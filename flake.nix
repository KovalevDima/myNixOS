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
    homeModules = [
      inputs.nix-colors.homeManagerModules.default
      ./home/communication.nix
      ./home/cli-tools.nix
      ./home/display.nix
      ./home/editor.nix
    ];
    systemModules = [
      ./system/network.nix
      ./system/display.nix
      ./system/gaming.nix
      ./system/i18n.nix
      ./system/unfreeSoftware.nix
      ./system/nix.nix
    ];
  in {
    nixosConfigurations = {
      laptop = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          (import ./hardware/laptop.nix)
          inputs.home-manager.nixosModules.home-manager
          (
            {inputs, config, pkgs, lib, ...} : {
              imports = systemModules;
              module.network = {
                enable = true;
                wan = "wlp1s0";
              };
              module.display =  {
                enable = true;
                initialUser = "dmitry";
              };
              module.gaming.enable = false;
              module.i18n.enable = true;
              module.unfreeSoftware.enable = true;
              module.nix.enable = true;

              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;

              users.users.dmitry = {
                isNormalUser = true;
                description = "dmitry";
                extraGroups = [ "networkmanager" "wheel" ];
              };

              environment.systemPackages = with pkgs; [
                google-chrome
                git
              ];

              time.timeZone = "Europe/Moscow";
              # Before changing this value read the documentation for this option
              # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
              system.stateVersion = "24.05"; # Did you read the comment?
            }
          )
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;

              users.dmitry = {
                imports = homeModules;
                programs.home-manager.enable = true;
                home = {
                  homeDirectory = "/home/dmitry";
                  stateVersion = "24.05";
                };
                colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;

                module.communication.enable = true;
                module.cli-tools.enable = true;
                module.display.enable = true;
                module.editor.enable = true;
              };
            };
          }
        ];
      };
    };
  };
}
