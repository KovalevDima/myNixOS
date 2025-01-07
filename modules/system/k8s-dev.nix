{ lib
, config
, pkgs
, ...
}:

{
  config = {
    environment.systemPackages = with pkgs; [
      # devops
      k9s
      kubernetes-helm
      kubectl
      yandex-cloud
      awscli2
      minikube
      postgresql
    ];

    virtualisation.docker.enable = true;
  };
}
