{ lib
, config
, pkgs
, ...
}:

{
  config = {
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
