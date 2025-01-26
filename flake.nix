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
  };

  outputs = { self, nixpkgs, disko, ... } @ inputs:
    let
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages = pkgs.haskellPackages;
      lib = inputs.nixpkgs.lib;
    in {
    nixosConfigurations = {
      desktop = lib.nixosSystem (import ./systems/desktop
        { inherit inputs;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            ./modules/system/hyprland.nix
            ./modules/system/nix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/nvim.nix
            ./modules/home/gui.nix
          ];
        }
      );
      laptop = lib.nixosSystem (import ./systems/laptop
        { inherit inputs;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            ./modules/system/hyprland.nix
            ./modules/system/nix.nix
          ];
          homeModules = [
            inputs.nix-colors.homeManagerModules.default
            ./modules/home/nvim.nix
            ./modules/home/gui.nix
          ];
        }
      );
      server = lib.nixosSystem (import ./systems/server
        { inherit inputs disko self;
          systemModules = [
            inputs.sops-nix.nixosModules.sops
            inputs.nix-bitcoin.nixosModules.default
            ./modules/system/nix.nix
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
        buildInputs = with haskellPackages; [
          haskell-language-server
          ghcid
          cabal-install
          pkgs.glslang
          pkgs.zlib
          pkgs.vulkan-loader
          pkgs.vulkan-headers
          pkgs.pkg-config
        ];
        inputsFrom = with self.packages.x86_64-linux; [ graphics compiler ];
      };
    };
    packages.x86_64-linux = {
      compiler = haskellPackages.callCabal2nix "compiler" ./personal-page {};
      graphics = pkgs.haskell.lib.addBuildDepends 
        (haskellPackages.callCabal2nix "graphics" ./graphics {}) [pkgs.glslang]; 
      personal-page = pkgs.stdenv.mkDerivation {
        name = "personal-page";

        src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;
        buildPhase = "${lib.getExe' self.packages.x86_64-linux.compiler "compiler"} build --verbose";

        installPhase = ''
          mkdir -p "$out"
          cp -r ./_site "$out"
        '';
      };
      image = let shaderPath = ./graphics/julia.glsl; in
        pkgs.writeShellScriptBin "mkImage" ''
          glslangValidator -S comp -V ${shaderPath} -o ./~compiledShader.spirv
          ${lib.getExe' self.packages.x86_64-linux.graphics "graphics"} ./~compiledShader.spirv
          rm ./~compiledShader.spirv
        '';
    };
  };
}
