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
        nixpkgs.overlays = [
          (final: prev: {
            amneziawg-go = prev.amneziawg-go.overrideAttrs (
              finalAttrs: prevAttrs: {
                name = "amneziawg-go";
                version = "0.2.13";

                src = prev.fetchFromGitHub {
                  owner = "amnezia-vpn";
                  repo = "amneziawg-go";
                  tag = "v${finalAttrs.version}";
                  hash = "sha256-vXSPUGBMP37kXJ4Zn5TDLAzG8N+yO/IIj9nSKrZ+sFA=";
                };

                vendorHash = "sha256-9OtIb3UQXpAA0OzPhDIdb9lXZQHHiYCcmjHAU+vCtpk=";
              }
            );
          })
        ];
        programs = {
          amnezia-vpn.enable = true;
          steam = {
            enable = true;
            remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
            dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
            localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
          };
        };
        networking = {
          hostName = "desktop";
          firewall.allowedTCPPorts = [3001];
        };
        services = {
          openssh = {
            enable = true;
            ports = [22];
            settings.AllowUsers = null;
          };
          xserver.videoDrivers = ["nvidia"];
        };
        hardware.nvidia = {
          modesetting.enable = true;
          nvidiaSettings = true;
          open = false;
        };
        boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_16;
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.kernelParams = [ "nvidia.NVreg_PreserveVideoMemoryAllocations=1" ];
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
          android-tools
          scrcpy
          # dev
          nix-tree
          postgresql
          yarn
          haskellPackages.haskell-language-server
          haskellPackages.ghc
          haskellPackages.cabal-install
          # system info
          btop
          smartmontools
          fastfetch
          # network
          chromium
          wget
          dig
          nmap
          openssl
          traceroute
          mtr
          # files processing
          unzip
          gnutar
          ffmpeg-full
          tree
          cloc
          filezilla
          shotcut
          # communication
          discord
          element-desktop
          telegram-desktop
          # fun
          cmatrix
          cbonsai
          cava
          # gaming
          prismlauncher
        ];
        virtualisation.docker.enable = true;
        nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
          "vscode"
          "yandex-cloud"
          "telegram-desktop"
          "steam"
          "steam-unwrapped"
          "nvidia-x11"
          "nvidia-settings"
          "discord"
        ];
        i18n.defaultLocale = "en_US.UTF-8";
        time.timeZone = "Europe/Moscow";
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        system.stateVersion = "25.11"; # Did you read the comment?
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
                enable = true;
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
              waybar = {
                enable = true;
                palette = theme.palette;
              };
              alacritty.palette = theme.palette;
              k9s.palette = theme.palette;
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
              stateVersion = "25.11";
              packages = [];
            };
          };
        };
      }
    )
  ];
}
