{ lib
, config
, pkgs
, inputs
, ...
}:

let inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) vimThemeFromScheme;
    in
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
        {
          plugin = vimThemeFromScheme {scheme = config.colorScheme;};
          config = "colorscheme nix-${config.colorScheme.slug}";
        }
      ];
    };
    programs.vscode = {
      enable = true;
    };
  };
}
