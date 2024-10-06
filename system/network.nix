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
      nameservers = [ "127.0.0.1" "::1" ];
      networkmanager = {
        enable = true;
        dns = "none";
      };
      resolvconf.enable = false;
      dhcpcd.enable = false;
    };
    services.resolved.enable = false;
    services.dnscrypt-proxy2 = {
      enable = true;
      settings = {
        cache = true;
        cache_max_ttl = 86400;
        cache_size = 1024;
        cert_refresh_delay = 240;
        doh_servers = true;
        ignore_system_dns = true;
        listen_addresses = [ "127.0.0.1:53" "[::1]:53" ];
        log_files_max_size = 10;
        keepalive = 30;
        require_dnssec = true;
        require_nofilter = true;
        require_nolog = true;
  
        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };
      };
    };
  
    systemd.services.dnscrypt-proxy2.serviceConfig = {
      StateDirectory = "dnscrypt-proxy";
      User = "root";
    };
  };
}
