{ inputs
, pkgs
, lib
, config
, username
, ...
}:

{
  options = {
    module.displayServer.enable = lib.mkEnableOption "Enables display server";
  };

  config = lib.mkIf config.module.displayServer.enable {
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

    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
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
      pkgs.kitty
    ];
  };
}
