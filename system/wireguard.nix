{ lib
, config
, pkgs
, ...
}:

let cfg = config.module.wireguard;
in {
  options = {
    module.wireguard.configFilepath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Wireguard preshared key";
    };
  };
  config = lib.mkIf (cfg.configFilepath != null) {
    networking = {
      wg-quick.interfaces.wg0.configFile = cfg.configFilepath;
    };
  };
}
