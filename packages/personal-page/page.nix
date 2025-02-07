{pkgs}:

pkgs.stdenv.mkDerivation {
  name = "personal-page";

  src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  installPhase = ''
    mkdir -p "$out"
    cp -r ./. "$out"
  '';
}
