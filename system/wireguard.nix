{ lib
, config
, pkgs
, ...
}:

let cfg = config.module.wireguard;
in {
  options = {
    module.wireguard.privateKeyFilepath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Wireguard private key";
    };
  };
  config = lib.mkIf (cfg.privateKeyFilepath != null) {
    networking = {
      firewall = {
        allowedUDPPorts = [ 51820 ];
      };

      wireguard.interfaces = {
        wg0 = {
          ips = [ "10.100.0.2/24" ];
          listenPort = 51820;
          privateKeyFile = cfg.privateKeyFilepath;
        };
      };
    };
  };
}
