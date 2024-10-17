{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.k8s-dev.enable = lib.mkEnableOption "Enables k8s development packages";
  };

  config = lib.mkIf config.module.k8s-dev.enable {
    environment.systemPackages = with pkgs; [
      # devops
      k9s
      kubernetes-helm
      kubectl
      yandex-cloud
      awscli2
    ];
  };
}
