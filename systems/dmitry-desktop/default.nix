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
      {inputs, config, pkgs, ...} : {
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
          hostName = "desktop";
          wg-quick.interfaces.wg0.configFile = "${config.sops.secrets."network/wireguardConfigFile".path}";
        };
        services.xserver.videoDrivers = ["nvidia"];
        services.udisks2.enable = true;
        services.openssh = {
          enable = true;
          ports = [22];
          settings.AllowUsers = null;
        };
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
          nix-tree
        ];
        time.timeZone = "Europe/Moscow";
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        system.stateVersion = "24.05"; # Did you read the comment?
      }
    )
    (
      {inputs, config, pkgs, ...} : {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {inherit inputs;};
          users.dmitry = {
            imports = [
              inputs.nix-colors.homeManagerModules.default
              ../../home/tui.nix
              ../../home/gui
              ../../home/gui/obs-studio.nix
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
                yarn
                nodejs
              ];
            };
            colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
            wayland.windowManager.hyprland.settings.monitor = [
              "DP-1, 2560x1440@165, 0x0, 1"
              "DP-2, 2560x1440@165, 2560x0, 1"
            ];
            module.tui.enable = true;
            module.gui.enable = true;
          };
        };
      }
    )
  ];
}
