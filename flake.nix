{
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs.follows = "unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix.url = "github:Mic92/sops-nix";
    nix-colors.url = "github:misterio77/nix-colors";
    nix-bitcoin.url = "github:fort-nix/nix-bitcoin";
    ClickHaskell.url = "github:KovalevDima/ClickHaskell";
  };

  outputs = { self, nixpkgs, disko, ... } @ inputs:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
    nixosConfigurations = {
      desktop = nixpkgs.lib.nixosSystem (import ./systems/desktop
        { inherit inputs;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            ./modules/system/hyprland.nix
            ./modules/system/nix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/hyprland.nix
            ./modules/home/k8s-dev.nix
            ./modules/home/waybar.nix
            ./modules/home/alacritty.nix
            ./modules/home/nvim.nix
          ];
        }
      );
      laptop = nixpkgs.lib.nixosSystem (import ./systems/laptop
        { inherit inputs;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            ./modules/system/hyprland.nix
            ./modules/system/nix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/hyprland.nix
            ./modules/home/alacritty.nix
            ./modules/home/waybar.nix
            ./modules/home/nvim.nix
          ];
        }
      );
      server = nixpkgs.lib.nixosSystem (import ./systems/server
        { inherit inputs disko self;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            inputs.nix-bitcoin.nixosModules.default
            inputs.ClickHaskell.nixosModules.default
            ./modules/system/nix.nix
            ./modules/system/mail-server.nix
            ./modules/system/minecraft.nix
            {nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
              "minecraft-server"
            ];}
            ./modules/system/matrix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/nvim.nix
          ];
        }
      );
    };
    devShells.x86_64-linux = {
      default = pkgs.mkShell {
        buildInputs = with pkgs.haskellPackages; [
          haskell-language-server
          ghcid
          cabal-install
          pkgs.nil
          pkgs.zlib
          pkgs.pkg-config
          pkgs.glslang
          pkgs.vulkan-tools
          pkgs.vulkan-loader
        ];
        inputsFrom = with self.packages.x86_64-linux; [image personal-page];
      };
    };
    packages.x86_64-linux = {
      personal-page = import ./packages/personal-page/page.nix {inherit pkgs;};
      image         = import ./packages/graphics/image.nix {inherit pkgs;};
    };
  };
}
