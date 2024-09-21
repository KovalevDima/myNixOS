{ lib
, config
, ...
}:

{
  options = {
    module.alacritty.enable = lib.mkEnableOption "Enables Alacritty";
  };

  config = lib.mkIf config.module.alacritty.enable {
    programs.alacritty = {
      enable = true;

      settings = {
        env.TERM = "xterm-256color";

        window = {
          # opacity = 0.95;

          dimensions = {
            lines = 27;
            columns = 115;
          };

          padding = {
            x = 4;
            y = 4;
          };
        };

        keyboard.bindings = [
          {
            action = "Copy";
            key = "C";
            mods = "Control";
          }
          {
            action = "Copy";
            key = "С";
            mods = "Control";
          }
          {
            action = "Paste";
            key = "V";
            mods = "Control";
          }
          {
            action = "Paste";
            key = "М";
            mods = "Control";
          }
          {
            action = "Paste";
            key = "Insert";
            mods = "Shift";
          }
          {
            action = "PasteSelection";
            key = "Insert";
            mods = "Shift";
          }
          {
            action = "PasteSelection";
            key = "Insert";
          }
          {
            chars = "\\u0003";
            key = "C";
            mods = "Control|Shift";
          }
          {
            chars = "\\u0003";
            key = "С";
            mods = "Control|Shift";
          }
        ];
      };
    };
  };
}
