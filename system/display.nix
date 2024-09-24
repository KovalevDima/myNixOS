{ inputs
, pkgs
, lib
, config
, username
, ...
}:

{
  options = {
    module.display.enable = lib.mkEnableOption "Enables display server";
  };

  config = lib.mkIf config.module.display.enable {
    services = {
      xserver = {
        enable = true;
        xkb.layout = "us,ru";
      };
      displayManager.autoLogin = {
        enable = true;
        user = "${username}";
      };
      libinput.enable = true;
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
      
    ];
  };
}
