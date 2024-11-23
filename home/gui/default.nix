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
    module.gui.enable = lib.mkEnableOption "Enables configuration for gui server";
  };

  imports = map (def: ./defaults/${def}) (builtins.attrNames (builtins.readDir ./defaults));

  config = lib.mkIf config.module.gui.enable {
    gtk = {
      enable = true;
      theme = {
        name = config.colorScheme.slug;
        package = gtkThemeFromScheme { scheme = config.colorScheme; };
      };
    };

  };
}
