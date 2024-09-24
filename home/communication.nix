{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.communication.enable = lib.mkEnableOption "Enables communication packages";
  };

  config = lib.mkIf config.module.communication.enable {
    home.packages = with pkgs; [
      discord
      telegram-desktop
    ];
  };
}
