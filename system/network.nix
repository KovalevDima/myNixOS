{ lib
, config
, pkgs
, ...
}:

let cfg = config.module.network;
in {
  options = {
    module.network.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to disable network";
    };
  };
  config = lib.mkIf cfg.enable {
    networking = {
      networkmanager.enable = true;
    };
  };
}
