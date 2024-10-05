{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.docker.enable = lib.mkEnableOption "Enables docker";
  };
  config = lib.mkIf config.module.docker.enable {
    virtualisation.docker.enable = true;
  };
}
