{ lib
, config
, pkgs
, inputs
, ...
}:
let
  inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) gtkThemeFromScheme;
in
{
  options = {
    module.hyprland.enable = lib.mkEnableOption "Enables configuration for display server";
  };

  config = lib.mkIf config.module.hyprland.enable {
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
              format = "{volume}% {icon} {format_source}";
              format-muted = "Muted  {format_source}";
              format-icons = {
                headphones = "";
                default = [ "" "" ];
              };
              format-source = "- ";
              format-source-muted = "- ";
            };

            "hyprland/workspaces" = { };
          };
        };
    
        style = with config.colorScheme.palette; ''
          .modules-left {
            background-color: #${base01};
            font-style: italic;
          }

          .modules-right {
            background-color: #${base01};
            font-style: italic;
          }
    
          #clock {
            padding: 4px;
            background-color: #${base01};
            color: #${base06};
          }

          #network,
          #battery,
          #pulseaudio {
            padding: 0 10px;
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
        '';
      };

      alacritty = {
        enable = true;
        settings = {
          env.TERM = "xterm-256color";
          colors = with config.colorScheme.palette;  {
            bright = {
              black   = "0x${base00}";
              blue    = "0x${base0D}";
              cyan    = "0x${base0C}";
              green   = "0x${base0B}";
              magenta = "0x${base0E}";
              red     = "0x${base08}";
              white   = "0x${base06}";
              yellow  = "0x${base09}";
            };
            cursor = {
              cursor = "0x${base06}";
              text   = "0x${base06}";
            };
            normal = {
              black   = "0x${base00}";
              blue    = "0x${base0D}";
              cyan    = "0x${base0C}";
              green   = "0x${base0B}";
              magenta = "0x${base0E}";
              red     = "0x${base08}";
              white   = "0x${base06}";
              yellow  = "0x${base0A}";
            };
            primary = {
              background = "0x${base00}";
              foreground = "0x${base06}";
            };
          };
          window = {
            opacity = 0.80;
            dimensions = {lines = 27; columns = 115;};
            padding = {x = 4; y = 4;};
          };
          keyboard.bindings = [
            {action = "Copy"; key = "C";mods = "Control";}
            {action = "Paste";key = "V";mods = "Control";}
            {action = "Paste";key = "М";mods = "Control";}
            {action = "PasteSelection";key = "Insert";mods = "Shift";}
            {chars = "\\u0003";key = "C";mods = "Control|Shift";}
          ];
        };
      };
    };

    gtk = {
      enable = true;
      theme = {
        name = config.colorScheme.slug;
        package = gtkThemeFromScheme { scheme = config.colorScheme; };
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      settings = {
        exec-once = [
          "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
          "swww-daemon & sleep 1 && swww img ${./display/wallpaper.jpeg}"
          "swaynotificationcenter"
        ];

        env = [
          "XCURSOR_SIZE, 24"
          "HYPRCURSOR_SIZE, 24"
        ];
        
        cursor = {
          allow_dumb_copy = true;
        };

        general = with config.colorScheme.palette; {
          gaps_in = 0;
          gaps_out = 0;

          border_size = 2;

          "col.active_border" = "rgba(${base00}ff)"; # "rgba(${base0E}ff) rgba(${base09}ff) 60deg";
          "col.inactive_border" = "rgba(${base00}ff)";

          resize_on_border = false;

          allow_tearing = false;

          layout = "dwindle";
        };

        # https://wiki.hyprland.org/Configuring/Variables/#decoration
        decoration = {
          rounding = 0;
          active_opacity = 1.0;
          inactive_opacity = 1.0;

          drop_shadow = true;
          shadow_range = 4;
          shadow_render_power = 3;
          "col.shadow" = "rgba(11111111)";

          blur = {
            enabled = true;
            size = 3;
            passes = 1;
            vibrancy = 0.1969;
          };
        };

        # https://wiki.hyprland.org/Configuring/Variables/#animations
        animations = {
          enabled = true;
          # Default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
          bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
          animation = [
            "windows, 1, 7, myBezier"
            "windowsOut, 1, 7, default, popin 80%"
            "border, 1, 10, default"
            "borderangle, 1, 8, default"
            "fade, 1, 7, default"
            "workspaces, 1, 6, default"
          ];
        };

        # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
        dwindle = {
          pseudotile = true; # Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          preserve_split = true; # You probably want this
        };
          
        # https://wiki.hyprland.org/Configuring/Variables/#misc
        misc = {
          force_default_wallpaper = -1; # Set to 0 or 1 to disable the anime mascot wallpapers
          disable_hyprland_logo = false; # If true disables the random hyprland logo / anime girl background. :(
        };

        # https://wiki.hyprland.org/Configuring/Variables/#input
        input = {
          kb_layout = "us,ru";
          kb_variant = "";
          kb_model = "";
          kb_options = "grp:win_space_toggle";
          kb_rules = "";

          follow_mouse = 1;

          sensitivity = 0; # from -1.0 to 1.0, 0 means no modification
        };

        # https://wiki.hyprland.org/Configuring/Variables/#gestures
        gestures = {
          workspace_swipe = false;
        };

        # See https://wiki.hyprland.org/Configuring/Keywords/
        "$mainMod" = "SUPER"; # Sets "Windows" key as main modifier

        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        bind = [
          "$mainMod, Q, exec, alacritty"
          "$mainMod, C, killactive,"
          "$mainMod, M, exit,"
          "$mainMod, D, exec, discord"
          "$mainMod, B, exec, google-chrome-stable"
          "$mainMod, E, exec, yazi"
          "$mainMod, V, togglefloating,"
          "$mainMod, R, exec, wofi --show drun"
          "$mainMod, P, pseudo," # dwindle
          "$mainMod, J, togglesplit," # dwindle
          "$mainMod, N, exec, swaync-client -t -sw"

          ", PRINT, exec, hyprshot -m region --clipboard-only"
          # Move focus with mainMod + arrow keys
          "$mainMod, left, movefocus, l"
          "$mainMod, right, movefocus, r"
          "$mainMod, up, movefocus, u"
          "$mainMod, down, movefocus, d"
          # Switch workspaces with mainMod + [0-9]
          "$mainMod, 1, workspace, 1"
          "$mainMod, 2, workspace, 2"
          "$mainMod, 3, workspace, 3"
          "$mainMod, 4, workspace, 4"
          "$mainMod, 5, workspace, 5"
          "$mainMod, 6, workspace, 6"
          "$mainMod, 7, workspace, 7"
          "$mainMod, 8, workspace, 8"
          "$mainMod, 9, workspace, 9"
          "$mainMod, 0, workspace, 10"
          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          "$mainMod SHIFT, 1, movetoworkspace, 1"
          "$mainMod SHIFT, 2, movetoworkspace, 2"
          "$mainMod SHIFT, 3, movetoworkspace, 3"
          "$mainMod SHIFT, 4, movetoworkspace, 4"
          "$mainMod SHIFT, 5, movetoworkspace, 5"
          "$mainMod SHIFT, 6, movetoworkspace, 6"
          "$mainMod SHIFT, 7, movetoworkspace, 7"
          "$mainMod SHIFT, 8, movetoworkspace, 8"
          "$mainMod SHIFT, 9, movetoworkspace, 9"
          "$mainMod SHIFT, 0, movetoworkspace, 10"
          # Example special workspace (scratchpad)
          "$mainMod, S, togglespecialworkspace, magic"
          "$mainMod SHIFT, S, movetoworkspace, special:magic"
          # Scroll through existing workspaces with mainMod + scroll
          "$mainMod, mouse_down, workspace, e+1"
          "$mainMod, mouse_up, workspace, e-1"
        ];

        # Move/resize windows with mainMod + LMB/RMB and dragging
        bindm = [
          "$mainMod, mouse:272, movewindow"
          "$mainMod, mouse:273, resizewindow"
        ];

        # Laptop multimedia keys for volume and LCD brightness
        bindel = [
          ",XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
          ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
          ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
          ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
        ];
        # Requires playerctl
        bindl = [
          ", XF86AudioNext, exec, playerctl next"
          ", XF86AudioPause, exec, playerctl play-pause"
          ", XF86AudioPlay, exec, playerctl play-pause"
          ", XF86AudioPrev, exec, playerctl previous"
        ];
        # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
        # See https://wiki.hyprland.org/Configuring/Workspace-Rules/ for workspace rules

        # Example windowrule v1
        # windowrule = float, ^(kitty)$

        # Example windowrule v2
        # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
        windowrulev2 = "suppressevent maximize, class:.*"; # You'll probably like this.
      };
    };      
  };
}
