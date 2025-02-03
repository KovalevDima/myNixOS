{ pkgs
, lib
, config
, ...
}:

{
  options = {
    module.gui = {
      initialUser = lib.mkOption {
        type = lib.types.str;
        default = "root";
        description = "Sets user to auto login";
      };
    };
  };

  config = {
    hardware.graphics.enable = true;

    services = {
      xserver = {
        enable = true;
        xkb.layout = "us,ru";
      };
      displayManager.autoLogin = {
        enable = true;
        user = "${config.module.gui.initialUser}";
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

    fonts.packages = [pkgs.nerd-fonts.hasklug];

    environment = {
      sessionVariables = {
        NIXOS_OZONE_WL = "1";
        SDL_VIDEODRIVER = "wayland";
        XDG_CURRENT_DESKTOP = "Hyprland";
        XDG_SESSION_TYPE = "wayland";
        XDG_SESSION_DESKTOP = "Hyprland";
        GDK_BACKEND = "wayland";
        CLUTTER_BACKEND = "wayland";
        WLR_RENDERER = "vulkan";
      };

      systemPackages = with pkgs; [
        swww
        wofi
        yazi
        swaynotificationcenter
        hyprshot
        amberol
        brightnessctl
      ];
    };

    programs = {
      xwayland.enable = true;
      hyprland.enable = true;
    };

    xdg.portal = {
      enable = true;
      xdgOpenUsePortal = true;
      extraPortals = [pkgs.xdg-desktop-portal-hyprland];
    };
  };
}
