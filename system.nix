{ inputs, config, pkgs, lib, ... }:


{
  imports = [
    ./system/network.nix
    ./system/zapret.nix
    ./system/displayServer.nix
  ];

# ==============
#   Boot
# ==============

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

# ==================
#   System modules
# ==================

  module.network.enable = true;
  module.zapret = {
    enable = true;
    wan = "wlp1s0";
  };
  module.displayServer.enable = true;

# ========================
#   Internationalisation
# ========================

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ru_RU.UTF-8";
    LC_IDENTIFICATION = "ru_RU.UTF-8";
    LC_MEASUREMENT = "ru_RU.UTF-8";
    LC_MONETARY = "ru_RU.UTF-8";
    LC_NAME = "ru_RU.UTF-8";
    LC_NUMERIC = "ru_RU.UTF-8";
    LC_PAPER = "ru_RU.UTF-8";
    LC_TELEPHONE = "ru_RU.UTF-8";
    LC_TIME = "ru_RU.UTF-8";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dmitry = {
    isNormalUser = true;
    description = "dmitry";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

# ============
#   Software
# ============

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "vscode"
    "yandex-cloud"
    "google-chrome"
    "telegram-desktop"
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # network
    google-chrome
    telegram-desktop
    
    # terminal tools
    htop
    neofetch
    gnutar

    # programming
    vscode
    yandex-cloud
    git
  ];

# ==========
#   System
# ==========

  time.timeZone = "Europe/Moscow";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}

