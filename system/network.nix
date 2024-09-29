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

    module.network.wan = lib.mkOption {
      type = lib.types.str;
      default = "eth0";
      description = "";
    };

    module.network.qnum = lib.mkOption {
      type = lib.types.int;
      default = 200;
      description = "";
    };
  };
  config = lib.mkIf cfg.enable {
    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;

    networking.hostName = "nixos";
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    networking.networkmanager.enable = true;

    networking = {
      firewall = {
        extraCommands = ''
          iptables -t mangle -I POSTROUTING -o "${cfg.wan}" -p tcp -m multiport --dports 80,443 -m connbytes --connbytes-dir=original --connbytes-mode=packets --connbytes 1:6 -m mark ! --mark 0x40000000/0x40000000 -j NFQUEUE --queue-num ${toString cfg.qnum} --queue-bypass
        '';
      };
    };
    systemd = {
      services = {
        zapret = {
          description = "zapret network service";
          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          path = with pkgs; [ 
            iptables
            zapret
            ipset
            curl
            wget
            gawk
          ];

          serviceConfig = {
            ExecStart = "${pkgs.zapret}/bin/nfqws --pidfile=/run/nfqws.pid --dpi-desync=fake,disorder --dpi-desync-ttl=5 --dpi-desync-fake-tls=0x00000000 --qnum=${toString cfg.qnum}";
            Type = "forking";
            PIDFile = "/run/nfqws.pid";
            ExecReload = "/bin/kill -HUP $MAINPID";
            Restart = "always";
            RestartSec = "5s";
          };
        };
      };
    };
  };
}
