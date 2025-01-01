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
  imports = map (def: ./defaults/${def}) (builtins.attrNames (builtins.readDir ./defaults));

  config = {
    gtk = {
      enable = true;
      theme = {
        name = config.colorScheme.slug;
        package = gtkThemeFromScheme { scheme = config.colorScheme; };
      };
    };
  };
}
