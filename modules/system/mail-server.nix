{ inputs
, pkgs
, lib
, config
, ...
}:

{
  options = {
    module.mail-server = {
      hostname = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Sets mail server domain";
      };
    };
  };
  
  config =
  let hostname = config.module.mail-server.hostname;
  in lib.mkIf (hostname != null) {
    users.groups.acme.members = [ "stalwart-mail" ];
    networking.firewall.allowedTCPPorts = [
      25 # smtp
      465 # submission tls
      993 # imap tls
      8050 # manage sieve
    ];

    services.stalwart-mail = {
      enable = true;
      settings =
      let certDir = config.security.acme.certs.${hostname}.directory;
      in {
        server = {
          hostname = "${hostname}";
          certificate.${hostname} = {
            cert        = "${certDir} + /fullchain.pem";
            private-key = "${certDir} + /key.pem";
          };
          listener = {
            smtp = {
              protocol = "smtp";
              bind = "[::]:25";
            };
            submissions = {
              protocol = "smtp";
              bind = "127.0.0.1:465";
              tls.implicit = true;
            };
            imaptls = {
              protocol = "imap";
              bind = "127.0.0.1:993";
              tls.implicit = true;
            };
            management = {
              protocol = "http";
              bind = "127.0.0.1:8050";
            };
          };

          storage = {
            blob = "rocksdb";
            data = "rocksdb";
            fts = "rocksdb";
            lookup = "rocksdb";
            directory = "internal";
          };

          directory."internal" = {
            store = "rocksdb";
            type = "internal";
          };

          store."rocksdb" = {
            compression = "lz4";
            path = "/var/lib/stalwart-mail/data";
            type = "rocksdb";
          };
        };
      };
    };
  };
}
