{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.networking.enable = lib.mkEnableOption "Enables network settings";
  };
  config = lib.mkIf config.module.networking.enable {
    
    networking.dhcpcd.enable = false;
    networking.useNetworkd = true;

    systemd.network = {
      enable = true;
      networks = {
        "10-wlan" = {
          matchConfig.Name = "wlan";
          networkConfig.DHCP = "ipv4";
        };
      };
    };
  };
}
