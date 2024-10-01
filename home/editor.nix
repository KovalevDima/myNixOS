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
      plugins = with pkgs.vimPlugins; [
        telescope-nvim
        (nvim-treesitter.withPlugins
          (p : [
            p.tree-sitter-nix
            p.tree-sitter-bash
            p.tree-sitter-json
            p.tree-sitter-yaml
            p.tree-sitter-markdown
          ])
        )
      ];
    };
    programs.vscode = {
      enable = true;
    };
  };
}
