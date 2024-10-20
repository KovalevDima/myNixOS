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
    homeModules = [
      inputs.nix-colors.homeManagerModules.default
      ./home/telegram.nix
      ./home/cli-tools.nix
      ./home/hyprland.nix
      ./home/editor.nix
      ./home/discord.nix
    ];
    systemModules = [
      inputs.sops-nix.nixosModules.sops
      ./system/hyprland.nix
      ./system/gaming.nix
      ./system/i18n.nix
      ./system/unfreeSoftware.nix
      ./system/nix.nix
      ./system/docker.nix
      ./system/wireguard.nix
      ./system/k8s-dev.nix
    ];
  in {
    nixosConfigurations = {
      desktop = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          (import ./hardware/desktop.nix)
          inputs.home-manager.nixosModules.home-manager
          (
            {inputs, config, pkgs, lib, ...} : {
              imports = systemModules;
              sops = {
                age.keyFile = "/root/.config/sops/age/keys.txt";
                defaultSopsFile = ./secrets.yaml;
                secrets = {
                  "network/wireguardConfigFile" = {};
                  "network/forwardingRules" = {};
                };
              };

              module.gaming.enable = true;
              module.i18n.enable = true;
              module.unfreeSoftware.enable = true;
              module.nix.enable = true;
              module.docker.enable = true;
              module.k8s-dev.enable = true;
              module.wireguard = {
                configFilepath = "${config.sops.secrets."network/wireguardConfigFile".path}";
              };
              module.hyprland =  {
                enable = true;
                initialUser = "dmitry";
              };

              networking.hostName = "desktop";
              services.xserver.videoDrivers = ["nvidia"];
              services.udisks2.enable = true;

              hardware.nvidia = {
                modesetting.enable = true;
                nvidiaSettings = true;
                open = false;
              };
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;

              users.users.dmitry = {
                isNormalUser = true;
                description = "dmitry";
                extraGroups = [ "networkmanager" "wheel" "docker" ];
              };

              environment.systemPackages = with pkgs; [
                google-chrome
                dig
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
              extraSpecialArgs = {inherit inputs;};

              users.dmitry = {
                imports = homeModules;
                programs.home-manager.enable = true;
                home = {
                  homeDirectory = "/home/dmitry";
                  stateVersion = "24.05";
                };
                colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
                wayland.windowManager.hyprland.settings.monitor = [
                  "DP-1, 2560x1440@165, 0x0, 1"
                  "DP-2, 2560x1440@165, 2560x0, 1"
                ];

                module.telegram.enable = true;
                module.discord.enable = true;
                module.cli-tools.enable = true;
                module.hyprland.enable = true;
                module.editor.enable = true;
              };
            };
          }
        ];
      };
      laptop = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          (import ./hardware/laptop.nix)
          inputs.home-manager.nixosModules.home-manager
          (
            {inputs, config, pkgs, lib, ...} : {
              imports = systemModules;

              module.gaming.enable = true;
              module.i18n.enable = true;
              module.unfreeSoftware.enable = true;
              module.nix.enable = true;
              module.docker.enable = true;
              module.hyprland =  {
                enable = true;
                initialUser = "dmitry";
              };
              networking.networkmanager.enable = true;

              networking.hostName = "nixos";
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;

              users.users.dmitry = {
                isNormalUser = true;
                description = "dmitry";
                extraGroups = [ "networkmanager" "wheel" "docker" ];
              };

              environment.systemPackages = with pkgs; [
                google-chrome
                dig
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
              extraSpecialArgs = {inherit inputs;};

              users.dmitry = {
                imports = homeModules;
                programs.home-manager.enable = true;
                home = {
                  homeDirectory = "/home/dmitry";
                  stateVersion = "24.05";
                };
                colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
                wayland.windowManager.hyprland.settings.monitor = ",preferred,auto,auto";

                module.telegram.enable = true;
                module.cli-tools.enable = true;
                module.hyprland.enable = true;
                module.editor.enable = true;
                module.discord.enable = true;
              };
            };
          }
        ];
      };
    };
  };
}
