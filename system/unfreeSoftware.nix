{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.unfreeSoftware.enable = lib.mkEnableOption "Enables unfree software";
  };
  config = lib.mkIf config.module.unfreeSoftware.enable {
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "vscode"
      "yandex-cloud"
      "google-chrome"
      "telegram-desktop"
      "discord"
      "steam"
      "steam-original"
      "steam-run"
      "steam-unwrapped"
      "nvidia-x11"
      "nvidia-settings"
    ];
  };
}
