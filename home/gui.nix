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
  imports = map (def: ./gui/${def}) (builtins.attrNames (builtins.readDir ./gui));

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
