{ lib
, config
, pkgs
, ...
}:
{
  options = {
    module.discord.enable = lib.mkEnableOption "Enables communication packages";
  };
  
  config = lib.mkIf config.module.discord.enable { 
    home.packages = [
        pkgs.vesktop
    ];
  };
}
