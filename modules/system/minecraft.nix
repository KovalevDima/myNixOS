{ lib
, config
, ...
}:

{
  options = {
    module.minecraft-server = {
      hostname = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Minecraft server hostname";
      };
    };
  };

  config =
    let
      hostname = config.module.minecraft-server.hostname;
    in
    lib.mkIf (hostname != null) {
      services.minecraft-server = {
        enable = true;
        eula = true;
        openFirewall = true;
        declarative = true;
        serverProperties = {
          server-port = 43000;
        };
      };

  };
}
