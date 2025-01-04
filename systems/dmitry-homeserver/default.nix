{ inputs
, systemModules
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
        networking = {
          hostName = "server";
        };
        services.udisks2.enable = true;
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
          ];
        };
        users.users.dmitry = {
          isNormalUser = true;
          description = "dmitry";
          extraGroups = [ "wheel" "docker" ];
        };
        environment.systemPackages = with pkgs; [
          dig
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
            ];
            home = {
              homeDirectory = "/home/dmitry";
              stateVersion = "24.05";
              packages = with pkgs; [
                yarn
                nodejs
              ];
            };
            colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
          };
        };
      }
    )
  ];
}
