{ config, pkgs, lib, ... }:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.dmitry = {
      imports = [
        (import ) 
      ];

      programs.home-manager.enable = true;

      home = {
        homeDirectory = "/home/dmitry";
        stateVersion = "24.05";
      };
    };
  };

}
