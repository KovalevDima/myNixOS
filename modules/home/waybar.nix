{ config
, lib
, ...
}:
{
  options = {
    module.waybar = {
      palette = lib.mkOption {
        type = lib.types.nullOr (lib.types.attrsOf lib.types.anything);
        default = null;
        description = "Waybar ricing palette";
      };
    };
  };

  config = 
  let
    palette = config.module.waybar.palette;
  in {
    programs = {
      waybar = {
        enable = true;
        systemd = {
          enable = true;
          target = "hyprland-session.target";
        };
        settings = {
          topbar = {
            layer = "top";
            height = 30;
            name = "topbar";
            modules-left = [ "hyprland/workspaces" ];
            modules-right = [
              "tray"
              "network"
              "pulseaudio"
              "battery"
              "clock"
            ];
            battery = {
              bat = "BAT1";
              format = "{capacity}% {icon}";
              format-icons = [ "" "" "" "" "" ];
            };
            clock = {
              interval = 1;
              format = "{:%I:%M %p}";
            };
            network = {
              "format-wifi" = "{essid} ({signalStrength}%) ";
              "format-ethernet" = "{ifname}";
            };
            pulseaudio = {
              format = "{volume}% {icon}  {format_source}";
              format-muted = "Muted  {format_source}";
              format-icons = {
                headphones = "";
                default = [ "" "" ];
              };
              format-source = "";
              format-source-muted = "";
            };

            "hyprland/workspaces" = { };
          };
        };

        style = with palette;
          lib.mkMerge [ ''
              .modules-left {font-style: italic;}
              .modules-right {font-style: italic;}
              #clock {padding: 4px;}

              #network,
              #battery,
              #pulseaudio {padding: 0 10px;}
            ''
            (lib.mkIf (palette != null) ''
              .modules-left {background-color: #${base01};}
              .modules-right {background-color: #${base01};}
              #clock {
                background-color: #${base01};
                color: #${base06};
              }

              #network,
              #battery,
              #pulseaudio {
                background-color: #${base01};
                color: #${base06};
              }

              #workspaces button {
                background-color: #${base01};
                color: #${base08};
              }

              #workspaces button.active {
                background-color: #${base02};
                color: #${base0B};
              }
              ''
            )
          ];
      };
    };
  };
}