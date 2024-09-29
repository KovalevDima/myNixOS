{ inputs
, pkgs
, lib
, config
, ...
}:

{
  options = {
    module.display = {
      enable = lib.mkEnableOption "Enables display server";
      initialUser = lib.mkOption {
        type = lib.types.str;
        default = "root";
        description = "Sets user to auto login";
      };
    };
  };

  config = lib.mkIf config.module.display.enable {

    hardware.graphics.enable = true;

    services = {
      xserver = {
        enable = true;
        xkb.layout = "us,ru";
      };
      displayManager.autoLogin = {
        enable = true;
        user = "${config.module.display.initialUser}";
      };
      libinput.enable = true;
      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        jack.enable = true;
      };
    };

    environment.sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };

    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
    };

    programs.waybar = {
      enable = true;
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      xdgOpenUsePortal = true;
      extraPortals = [
        inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland
      ];
    };

    environment.systemPackages = [
      inputs.swww.packages.${pkgs.system}.swww
      pkgs.rofi-wayland
    ];
  };
}
