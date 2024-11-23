{ lib
, config
, pkgs
, inputs
, ...
}:

let
  inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) vimThemeFromScheme;
in
{
  options = {
    module.cli-tools.enable = lib.mkEnableOption "Enables CLI tools packages";
  };

  config = lib.mkIf config.module.cli-tools.enable {
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
            p.tree-sitter-haskell
          ])
        )
        {
          plugin = vimThemeFromScheme {scheme = config.colorScheme;};
          config = "colorscheme nix-${config.colorScheme.slug}";
        }
      ];
    };

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
