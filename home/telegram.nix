{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.telegram.enable = lib.mkEnableOption "Enables telegram";
  };

  config = lib.mkIf config.module.telegram.enable {
    home.packages = with pkgs; [
      telegram-desktop
    ];
  };
}
