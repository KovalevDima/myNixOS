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

    fonts.packages = [
      pkgs.nerdfonts
    ];

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
      ];
    };

    programs = {
      xwayland.enable = true;
      hyprland.enable = true;
    };

    xdg = {
      portal = {
        enable = true;
        xdgOpenUsePortal = true;
        extraPortals = [
          pkgs.xdg-desktop-portal-hyprland
        ];
      };
    };
  };
}
