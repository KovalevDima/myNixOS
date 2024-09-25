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
      # devops
      k9s
      kubernetes-helm
      kubectl
      yandex-cloud
      # system info
      neofetch
      btop
      # fun
      cmatrix
      # compression
      gnutar
    ];
  };
}
