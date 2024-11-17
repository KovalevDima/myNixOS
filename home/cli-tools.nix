{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.cli-tools.enable = lib.mkEnableOption "Enables CLI tools packages";
  };

  config = lib.mkIf config.module.cli-tools.enable {
    home.packages = with pkgs; [
      # system info
      btop
      fastfetch

      # fun
      cmatrix
      cbonsai
      ### cava

      # files processing
      gnutar
      ffmpeg-full

      # network
      wget
      tcpdump
      tcpflow
      termshark
    ];
  };
}
