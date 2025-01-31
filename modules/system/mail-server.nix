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
      mailServerSecret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Sets mail server admin secret";
      };
    };
  };
  
  config =
  let
    hostname = config.module.mail-server.hostname;
    mailServerSecret = config.module.mail-server.mailServerSecret;
  in lib.mkIf (hostname != null) {
    users.groups.acme.members = [ "stalwart-mail" ];
    services.stalwart-mail = {
      enable = true;
      openFirewall = true;
      settings =
      let certDir = config.security.acme.certs.${hostname}.directory;
      in {
        certificate.default = {
          default = true;
          cert        = "%{file:${certDir}/cert.pem}%";
          private-key = "%{file:${certDir}/key.pem}%";
        };
        lookup.default.hostname = "mail.${hostname}";
        lookup.default.domain = "${hostname}";
        server = {
          hostname = "mail.${hostname}";
          tls = {
            enable = true;
            implicit = true;
          };
          listener = {
            smtp = {
              protocol = "smtp";
              bind = "0.0.0.0:25";
            };
            submissions = {
              protocol = "smtp";
              bind = "0.0.0.0:465";
              tls.implicit = true;
            };
            imaptls = {
              protocol = "imap";
              bind = "0.0.0.0:993";
              tls.implicit = true;
            };
            management = {
              protocol = "http";
              bind = "0.0.0.0:8050";
            };
          };
        };
        directory."memory" = {
          type = "memory";
          principals = [
            {
              name = "dmitry";
              class = "individual";
              email = [ "dmitry@boot.directory" ];
              secret = "{plain}%{file:${mailServerSecret}}%";
            }
          ];
        };
        tracer."stdout" = {
          type = "stdout";
          level = "info";
          ansi = false;
          enable = true;
        };
        authentication = {
          fallback-admin = {
            user = "admin";
            secret = "{plain}%{file:${mailServerSecret}}%";
          };
          master = {
            user = "master";
            secret = "%{file:${mailServerSecret}}%";
          };
        };
      };
    };
  };
}
