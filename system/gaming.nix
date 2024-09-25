{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.gaming.enable = lib.mkEnableOption "Enables CLI tools packages";
  };
  config = lib.mkIf config.module.gaming.enable {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    };
  };
}
