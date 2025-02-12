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
          # dev
          nix-tree
          k9s
          kubernetes-helm
          kubectl
          yandex-cloud
          awscli2
          minikube
          postgresql
          yarn
          nodejs
          # system info
          btop
          fastfetch
          # network
          wget
          dig
          # files processing
          unzip
          gnutar
          ffmpeg-full
          tree
          # communication
          discord
          element-desktop
          telegram-desktop
          # fun
          cmatrix
          cbonsai
          cava
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
      {inputs, config, pkgs, ...}:
      let
        theme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
        nixColors = inputs.nix-colors.lib-contrib {inherit pkgs;};
      in
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {inherit inputs;};
          users.dmitry = {
            imports = homeModules;
            colorScheme = theme;
            module = {
              hyprland = {
                palette = theme.palette;
                monitors = [
                  "DP-1, 2560x1440@165, 0x0, 1"
                  "DP-2, 2560x1440@165, 2560x0, 1"
                ];
                wallpaper = nixColors.nixWallpaperFromScheme {
                  scheme = theme;
                  width = 2560;
                  height = 1440;
                  logoScale = 15.0;
                };
                gtkTheme = {
                  name = theme.slug;
                  package = nixColors.gtkThemeFromScheme { scheme = theme; };
                };
              };
              waybar.palette = theme.palette;
              alacritty.palette = theme.palette;
              nvim.theme ={
                plugin = nixColors.vimThemeFromScheme {scheme = theme;};
                config = "colorscheme nix-${theme.slug}";
              };
            };
            programs = {
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
              packages = [];
            };
          };
        };
      }
    )
  ];
}
