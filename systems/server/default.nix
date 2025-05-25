{ inputs
, self
, systemModules
, homeModules
, disko
, ...
}:
{
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    disko.nixosModules.disko
    (import ./disk-config.nix)
    (import ./hardware.nix)
    inputs.home-manager.nixosModules.home-manager
    (
      {inputs, config, pkgs, ...} : {
        imports = systemModules;
        sops = {
          age.keyFile = "/root/.config/sops/age/keys.txt";
          defaultSopsFile = ../../secrets.yaml;
          secrets = {
            "matrix/sharedSecret" = { owner = "matrix-synapse"; };
            "keycloak/admin-realm" = { owner = "keycloak"; };
            "keycloak/databasePass" = { owner = "keycloak"; };
          #  "mailServerSecret" = { owner="stalwart-mail"; };
          };
        };
        # module.mail-server = {
        #   hostname = "boot.directory";
        #   mailServerSecret = "${config.sops.secrets."mailServerSecret".path}";
        # };
        # users.users.stalwart-mail.extraGroups = [ "acme" ];
        module.matrix.shared_secret = "${config.sops.secrets."matrix/sharedSecret".path}";
        module.minecraft-server.hostname = "mc.${config.networking.domain}";
        services.nginx = {
          enable = true;
          virtualHosts = {
            "${config.networking.domain}" = {
              enableACME = true;
              forceSSL = true;
              root = "${self.packages.x86_64-linux."personal-page"}";
            };
            "auth.${config.networking.domain}" = {
              forceSSL = true;
              useACMEHost = "${config.networking.domain}";
              locations."/".proxyPass =
                let kcPort = config.services.keycloak.settings.http-port;
                in "http://127.0.0.1:${toString kcPort}";
            };
            "${config.ClickHaskell.domain}" = {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://unix:${config.ClickHaskell.path}/ClickHaskell.sock:";
                extraConfig = ''
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "Upgrade";
                  proxy_set_header Host $host;
                  proxy_set_header X-Real-IP $remote_addr;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                '';
              };
            };
            "git.${config.ClickHaskell.domain}" = {
              forceSSL = true;
              useACMEHost = "${config.ClickHaskell.domain}";
              locations."/" = {
                return = "301 https://github.com/KovalevDima/ClickHaskell";
              };
            };
          };
        };
        users.users.nginx.extraGroups = [ config.ClickHaskell.group "acme" ];
        security.acme = {
          acceptTerms = true;
          certs = {
            "${config.networking.domain}" = {
              email = "letsencrypt@${config.networking.domain}";
              group = "acme";
              extraDomainNames = [ "auth.${config.networking.domain}" ];
            };
          };
        };
        networking.hostName = "server";
        networking.domain = "boot.directory";
        networking.firewall.allowedTCPPorts = [ config.services.btcpayserver.port 80 443 ];
        services.openssh = {
          enable = true;
          ports = [22];
          settings.AllowUsers = null;
        };
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        users.users.root = {
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAMRId+WDlD6u83HZx62o0PrCS0aZSnSJT5kXbKI9CaV dmitry@desktop"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKlwdqhLRKjVCv6+DMxw3GiOCE2qK6o9I8Ed9OTTwVQG dmitry@nixos"
          ];
        };
        services.keycloak = {
          enable = false;
          database.passwordFile = "${config.sops.secrets."keycloak/databasePass".path}";
          settings = {
            hostname = "auth.${config.networking.domain}";
            http-port = 9080;
            http-host = "127.0.0.1";
            http-enabled = true;
            proxy-headers = "xforwarded";
          };
        };
        users.users.keycloak = { isNormalUser = true; };
        services.bitcoind = {
          enable = true;
          prune = 10000;
        };
        services.btcpayserver = {
          enable = true;
          address = "0.0.0.0";
        };
        nix-bitcoin = {
          nodeinfo.enable = true;
          generateSecrets = true;
          operator = {
            enable = true;
            name = "dmitry";
          };
        };
        users.users.dmitry = {
          isNormalUser = true;
          description = "dmitry";
          extraGroups = [ "wheel" "docker" ];
        };
        environment.systemPackages = with pkgs; [
          dig
          # system info
          btop
          smartmontools
          fastfetch
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
            module = {
              nvim.theme = {
                plugin = nixColors.vimThemeFromScheme {scheme = theme;};
                config = "colorscheme nix-${theme.slug}";
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
