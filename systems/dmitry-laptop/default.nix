{ inputs
, systemModules
, ...
}:
{
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    (import ./hardware.nix)
    inputs.home-manager.nixosModules.home-manager
    (
      {inputs, config, pkgs, lib, ...} : {
        imports = systemModules;
        sops = {
          age.keyFile = "/root/.config/sops/age/keys.txt";
          defaultSopsFile = ../dmitry-secrets.yaml;
          secrets = {
            "network/wireguardConfigFile" = {};
          };
        };
        module.gaming.enable = true;
        module.i18n.enable = true;
        module.unfreeSoftware.enable = true;
        module.nix.enable = true;
        module.k8s-dev.enable = true;
        module.gui =  {
          enable = true;
          initialUser = "dmitry";
        };
        networking = {
          hostName = "nixos";
          networkmanager.enable = true;
          networkmanager.dns = "none";
          useDHCP = false;
          dhcpcd.enable = false;
          dhcpcd.extraConfig = "nohook resolv.conf";
          nameservers = [ "8.8.8.8" "8.8.4.4"];
          wg-quick.interfaces.wg0.configFile = "${config.sops.secrets."network/wireguardConfigFile".path}";
        };
        services.resolved.enable = false;
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
    (
      {inputs, config, pkgs, lib, ...} : {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {inherit inputs;};
          users.dmitry = {
            imports =  [
              inputs.nix-colors.homeManagerModules.default
              ../../home/tui.nix
              ../../home/gui
            ];
            programs = {
              home-manager.enable = true;
              vscode.enable = true;
            };
            home = {
              homeDirectory = "/home/dmitry";
              stateVersion = "24.05";
              packages = with pkgs; [
                telegram-desktop
                vesktop
              ];
            };
            colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
            wayland.windowManager.hyprland.settings.monitor = ",preferred,auto,auto";
            module.tui.enable = true;
            module.gui.enable = true;
          };
        };
      }
    )
  ];
}