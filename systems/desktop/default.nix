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
      {inputs, config, pkgs, lib, ...}:{
        imports = systemModules;
        sops = {
          age.keyFile = "/home/dmitry/.config/sops/age/keys.txt";
          defaultSopsFile = ../../secrets.yaml;
          secrets = {
            "network/wireguardConfigFile" = {};
          };
        };
        module.gui.initialUser = "dmitry";
        programs = {
          gnome-disks.enable = true;
          amnezia-vpn.enable = true;
          steam = {
            enable = true;
            remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
            dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
            localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
          };
        };
        systemd.tmpfiles.rules = [
          "L+ /bin/bash - - - - /run/current-system/sw/bin/bash"
        ];
        # systemd.network.enable = true;
        # networking.useNetworkd = true;
        networking = {
          firewall.enable = false;
          hostName = "desktop";
          networkmanager.enable = true;
          firewall.allowedTCPPorts = [ 443 3001 ];
          firewall.allowedUDPPorts = [ 443 3001 ];
        };
        services = {
          llama-cpp = {
            enable = false;
            package = pkgs.llama-cpp-rocm;
            # extraFlags = ["--models-dir" "/etc/models/"];
          };
          lact.enable = true;
          pcscd.enable = true;
          udev = {
            packages = [
              pkgs.yubikey-personalization
              pkgs.libfido2
            ];
            extraRules = ''
              # RS-Key own identity (pid.codes 0x1209:0x0001) — FIDO HID + CCID access.
              SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="0001", TAG+="uaccess"
              SUBSYSTEM=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="0001", TAG+="uaccess"
            '';
          };
          zapret-discord-youtube = {
            enable = true;
            configName = "general(ALT13)";
          };
          openssh = {
            enable = true;
            ports = [22];
            settings.AllowUsers = null;
          };
          xserver.videoDrivers = ["amdgpu"];
          udisks2.enable = true;
        };

        security.polkit.extraConfig = ''
          polkit.addRule(function(action, subject) {
            if ((action.id == "org.debian.pcsc-lite.access_pcsc" ||
                 action.id == "org.debian.pcsc-lite.access_card") &&
                subject.user == "dmitry") {
              return polkit.Result.YES;
            }
          });
        '';

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.extraModulePackages = with config.boot.kernelPackages; [ zenpower ];
        boot.kernelModules = [ "zenpower" ];
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
        environment.etc."/ansible/ansible.cfg".text = ''
          [defaults]
          interpreter_python = /run/current-system/sw/bin/python3
        '';
        environment.sessionVariables.TZ = config.time.timeZone;
        environment.systemPackages = with pkgs;
          let
            python =
              python3.withPackages (ps: with ps; [
                pytz
                hvac
                psycopg2
              ]);
          in [
          ##########################################
          # dev
          ##########################################
          autoconf
          jq
          nodejs
          pkg-config
          gnumake
          coreutils
          bash
          gcc
          gh
          nix-tree
          postgresql
          zookeeper
          clickhouse
          yarn
          zlib
          haskellPackages.haskell-language-server
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.eventlog2html
          qemu
          ##########################################
          # system info
          ##########################################
          perf
          zenmonitor
          mesa-demos
          pciutils
          amdgpu_top
          btop
          smartmontools
          fastfetch
          hwinfo
          dmidecode
          usbutils
          ##########################################
          # network
          ##########################################
          iptables
          chromium
          wget
          dig
          nmap
          openssl
          traceroute
          mtr
          pritunl-client
          snx-rs
          iperf
          ##########################################
          # files processing
          ##########################################
          # wpsoffice
          unzip
          gnutar
          ffmpeg-full
          tree
          cloc
          filezilla
          # shotcut
          ##########################################
          # communication
          ##########################################
          discord
          element-desktop
          telegram-desktop
          slack
          pulseaudio
          ##########################################
          # fun
          ##########################################
          cmatrix
          cbonsai
          cava
          prismlauncher
          ##########################################
          # job
          ##########################################
          code-cursor
          # (ansible.override {inherit python;})
          # pipenv
          python
          python3Packages.pip
          pyenv
          # vault
          # terraform
          ##########################################
          # personal
          ##########################################
          yubikey-manager
          libfido2
          opensc
          pcsc-tools
          picotool
        ];
        virtualisation.docker.enable = true;
        nixpkgs.config = {
          allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
            "vscode"
            "yandex-cloud"
            "telegram-desktop"
            "steam"
            "steam-unwrapped"
            "discord"
            "discord-unwrapped"
            "wpsoffice"
            "slack"
            "pritunl-client"
            "terraform"
            "vault"
            "cursor"
          ];
          permittedInsecurePackages = [
            "gradle-7.6.6"
          ];
        };
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
              mangohud.enable = true;
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
              stateVersion = "26.05";
              packages = [];
            };
          };
        };
      }
    )
  ];
}
