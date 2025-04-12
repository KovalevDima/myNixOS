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

    programs.hyprland.enable = true;

    xdg.portal.enable = true;

    services = {
      xserver.enable = true;
      displayManager = {
        autoLogin = {
          enable = true;
          user = "${config.module.gui.initialUser}";
        };
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
        EDITOR = "nvim";
        ELECTRON_OZONE_PLATFORM_HINT = "wayland";
        MOZ_ENABLE_WAYLAND = "1";
        MOZ_DBUS_REMOTE="1";
        NIXOS_OZONE_WL = "1";
        SDL_VIDEODRIVER = "wayland";
        XDG_CURRENT_DESKTOP = "Hyprland";
        XDG_SESSION_TYPE = "wayland";
        XDG_SESSION_DESKTOP = "Hyprland";
        GDK_BACKEND = "wayland";
        GDK_GL = "disable";
        CLUTTER_BACKEND = "wayland";
        WLR_RENDERER = "vulkan";
        NVD_BACKEND = "direct";
      };

      systemPackages = with pkgs; [
        swww
        wofi
        swaynotificationcenter
        hyprshot
        amberol
        brightnessctl
      ];
    };
  };
}
