{ inputs
, pkgs
, lib
, config
, ...
}:

{
  options = {
    module.hyprland = {
      enable = lib.mkEnableOption "Enables display server";
      initialUser = lib.mkOption {
        type = lib.types.str;
        default = "root";
        description = "Sets user to auto login";
      };
    };
  };

  config = lib.mkIf config.module.hyprland.enable {

    hardware.graphics.enable = true;

    services = {
      xserver = {
        enable = true;
        xkb.layout = "us,ru";
      };
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = false;
      };
      displayManager.autoLogin = {
        enable = true;
        user = "${config.module.hyprland.initialUser}";
      };
      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        jack.enable = true;
        wireplumber.enable = true;
      };
    };

    environment.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      SDL_VIDEODRIVER = "wayland,windows";
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "Hyprland";
      GDK_BACKEND = "wayland";
      CLUTTER_BACKEND = "wayland";
      WLR_RENDERER = "vulkan";
    };

    programs = {
      xwayland= {
        enable = true;
      };
      waybar = {
        enable = false;
      };
      hyprland = {
        enable = true;
        package = inputs.hyprland.packages.${pkgs.system}.hyprland;
        portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
      };
    };

    xdg.portal = {
      enable = true;
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
