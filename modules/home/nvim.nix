{ config
, pkgs
, lib
, ...
}:

{
  options = {
    module.nvim = {
      theme = lib.mkOption {
        type = lib.types.nullOr (lib.types.attrsOf lib.types.anything);
        default = null;
        description = "Custom theme for neovim";
         # ToDo: Pass to systems configurations
         # inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) vimThemeFromScheme;
         # a = vimThemeFromScheme {scheme = config.colorScheme;};
      };
    };
  };
  config =
  let
    theme = config.module.nvim.theme;
  in {
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
        (pkgs.lib.mkIf (theme != null)
          {
            plugin = theme;
            config = "colorscheme nix-${config.colorScheme.slug}";
          }
        )
      ];

      extraConfig = ''
        " Tabs/spaces
        set tabstop=2
        set expandtab
        set shiftwidth=2

        " Navigation
        set scrolloff=3             " some lines around scroll for context

        " Cursor/Line
        set number
        set relativenumber
        set colorcolumn=-0          " based on textwidth
        set cursorline              " highlight the current line

        " Status/History
        set history=200             " remember a lot of stuff
        set ruler                   " Always show info along bottom.
        set cmdheight=1

        " Scrolling
        set ttyfast

        " Files
        set autoread                            " auto-reload files changed on disk
        set updatecount=0                       " disable swap files
        set wildmode=longest,list,full 

        " Vimdiff
        set diffopt=filler,vertical

        " Conceal (disabled by default)
        set conceallevel=0

        " Wrapping
        set nowrap

        " Leader
        nnoremap <Space> <Nop>
        let mapleader = ' '
        let maplocalleader = ' '

        " Make F1 work like Escape.
        map <F1> <Esc>
        imap <F1> <Esc>

        " Mouse issue (https://github.com/neovim/neovim/wiki/Following-HEAD#20170403)
        set mouse=a

        " Use system clipboard for yanks.
        set clipboard+=unnamedplus

        " Use ,t for 'jump to tag'.
        nnoremap <Leader>t <C-]>

        " Allow hidden windows
        set hidden

        " Grep with rg
        set grepprg=rg\ --line-number\ --column
        set grepformat=%f:%l:%c:%m

        " Theme
        set termguicolors
      '';
    };
  };
}
