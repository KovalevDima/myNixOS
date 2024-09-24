{ lib
, config
, pkgs
, ...
}:

{
  options = {
    module.editor.enable = lib.mkEnableOption "Enables editor";
  };

  config = lib.mkIf config.module.editor.enable {
    programs.neovim = {
      enable = true;
    };
    programs.vscode = {
      enable = true;
    };
  };
}
