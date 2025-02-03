{ inputs
, systemModules
, homeModules
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
          defaultSopsFile = ../../secrets.yaml;
          secrets = {
            "network/wireguardConfigFile" = {};
          };
        };
        module.gui.initialUser = "dmitry";
        programs = {
          amnezia-vpn.enable = true;
          steam = {
            enable = true;
            remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
            dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
            localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
          };
        };
        networking.hostName = "desktop";
        services = {
          openssh = {
            enable = true;
            ports = [22];
            settings.AllowUsers = null;
          };
          xserver.videoDrivers = ["nvidia"];
          udisks2.enable = true;
        };
        hardware.nvidia = {
          modesetting.enable = true;
          nvidiaSettings = true;
          open = false;
        };
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        users.users = {
          dmitry = {
            isNormalUser = true;
            description = "dmitry";
            extraGroups = [ "networkmanager" "wheel" "docker" ];
            openssh.authorizedKeys.keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKlwdqhLRKjVCv6+DMxw3GiOCE2qK6o9I8Ed9OTTwVQG dmitry@nixos"
            ];
          };
        };
        environment.systemPackages = with pkgs; [
          dig
          nix-tree
          k9s
          kubernetes-helm
          kubectl
          yandex-cloud
          awscli2
          minikube
          postgresql
          element-desktop
        ];
        virtualisation.docker.enable = true;
        nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
          "vscode"
          "yandex-cloud"
          "telegram-desktop"
          "discord"
          "steam"
          "steam-original"
          "steam-run"
          "steam-unwrapped"
          "nvidia-x11"
          "nvidia-settings"
        ];
        i18n.defaultLocale = "en_US.UTF-8";
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
            imports = homeModules;
            programs = {
              home-manager.enable = true;
              vscode.enable = true;
              firefox.enable = true; 
              obs-studio = {
                enable = true;
                plugins = with pkgs.obs-studio-plugins; [
                  wlrobs
                  obs-backgroundremoval
                  obs-pipewire-audio-capture
                ];
              };
            };
            home = {
              homeDirectory = "/home/dmitry";
              stateVersion = "24.05";
              packages = with pkgs; [
                telegram-desktop
                vesktop
                yarn
                nodejs
                # system info
                btop
                fastfetch
                # fun
                cmatrix
                cbonsai
                cava
                # files processing
                gnutar
                ffmpeg-full
                # network
                wget
              ];
            };
            colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
            wayland.windowManager.hyprland.settings.monitor = [
              "DP-1, 2560x1440@165, 0x0, 1"
              "DP-2, 2560x1440@165, 2560x0, 1"
            ];
          };
        };
      }
    )
  ];
}
