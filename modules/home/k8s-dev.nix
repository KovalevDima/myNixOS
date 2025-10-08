{ config
, pkgs
, lib
, ...
}:
let
  palette = config.module.k9s.palette;
in 
{
  options = {
    module.k9s = {
      palette = lib.mkOption {
        type = lib.types.nullOr (lib.types.attrsOf lib.types.anything);
        default = null;
        description = "k9s ricing palette";
      };
    };
  };
  config = {
    home.packages = with pkgs; [
      minikube
      k9s
      kubernetes-helm
      kubectl
      yandex-cloud
      # awscli2
    ];
    programs = with palette; {
      k9s = {
        enable = true;
        settings = {
          k9s = {
            ui = {
              skin = lib.mkIf false "generated_theme";
            };
          };
        };
        skins =  {
          generated_theme = lib.mkIf (palette != null) {
            k9s = {
              body = {
                fgColor = "#${base05}";
                bgColor = "default";
                logoColor = "#${base0C}";
              };

              prompt = {
                fgColor = "#${base05}";
                bgColor = "#${base00}";
                suggestColor = "#${base0A}";
              };

              info = {
                fgColor = "#${base0B}";
                sectionColor = "#${base05}";
              };

              dialog = {
                fgColor = "#${base05}";
                bgColor = "default";
                buttonFgColor = "#${base05}";
                buttonBgColor = "#${base0C}";
                buttonFocusFgColor = "#${base0E}";
                buttonFocusBgColor = "#${base0B}";
                labelFgColor = "#${base0A}";
                fieldFgColor = "#${base05}";
              };

              frame = {
                border = {
                  fgColor = "#${base02}";
                  focusColor = "#${base01}";
                };

                menu = {
                  fgColor = "#${base05}";
                  keyColor = "#${base0B}";
                  numKeyColor = "#${base0B}";
                };

                crumbs = {
                  fgColor = "#${base05}";
                  bgColor = "#${base01}";
                  activeColor = "#${base01}";
                };

                status = {
                  newColor = "#${base08}";
                  modifyColor = "#${base0C}";
                  addColor = "#${base09}";
                  errorColor = "#${base0D}";
                  highlightcolor = "#${base0A}";
                  killColor = "#${base03}";
                  completedColor = "#${base03}";
                };

                title = {
                  fgColor = "#${base05}";
                  bgColor = "#${base01}";
                  highlightColor = "#${base0A}";
                  counterColor = "#${base0C}";
                  filterColor = "#${base0B}";
                };
              };

              views = {
                charts = {
                  bgColor = "default";
                  defaultDialColors = [
                    "#${base0C}"
                    "#${base0D}"
                  ];
                  defaultChartColors = [
                    "#${base0C}"
                    "#${base0D}"
                  ];
                };

                table = {
                  fgColor = "#${base05}";
                  bgColor = "default";
                  header = {
                    fgColor = "#${base05}";
                    bgColor = "default";
                    sorterColor = "#${base08}";
                  };
                };

                xray = {
                  fgColor = "#${base05}";
                  bgColor = "default";
                  cursorColor = "#${base01}";
                  graphicColor = "#${base0C}";
                  showIcons = false;
                };

                yaml = {
                  keyColor = "#${base0B}";
                  colonColor = "#${base0C}";
                  valueColor = "#${base05}";
                };

                logs = {
                  fgColor = "#${base05}";
                  bgColor = "default";
                  indicator = {
                    fgColor = "#${base05}";
                    bgColor = "#${base0C}";
                  };
                };

                help = {
                  fgColor = "#${base05}";
                  bgColor = "#${base00}";
                  indicator.fgColor = "#${base0D}";
                };
              };
            };
          };
        };
      };
    };
  };
}
