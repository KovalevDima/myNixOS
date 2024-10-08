{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.nix.enable = lib.mkEnableOption "Enables nix settings";
  };
  config = lib.mkIf config.module.nix.enable {
    nix = {
      extraOptions = "experimental-features = nix-command flakes";
    };

    environment.systemPackages = with pkgs; [
      sops
      age
      git
    ];
  };
}
