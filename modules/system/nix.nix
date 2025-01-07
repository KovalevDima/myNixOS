{ lib
, config
, pkgs
, ...
}:

{
  config = {
    nix = {
      extraOptions = "experimental-features = nix-command flakes";
    };

    environment.systemPackages = with pkgs; [sops age git];

    programs = {
      gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
      direnv = {
        enable = true;
        loadInNixShell = true;
        nix-direnv.enable = true;
      };
    };
  };
}
