{ config, pkgs, lib, ... }:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.dmitry = {
      imports = [
        ./home/alacritty.nix
      ];

      module.alacritty.enable = true;

      programs.home-manager.enable = true;

      home = {
        homeDirectory = "/home/dmitry";
        stateVersion = "24.05";
      };
    };
  };

}
