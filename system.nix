{ inputs, config, pkgs, lib, ... }:

{
  imports = [
    ./system/network.nix
    ./system/display.nix
    ./system/gaming.nix
    ./system/i18n.nix
    ./system/unfreeSoftware.nix
  ];

  module.network = {
    enable = true;
    wan = "wlp1s0";
  };
  module.display.enable = true;
  module.gaming.enable = true;
  module.i18n.enable = true;
  module.unfreeSoftware.enable = true;

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

