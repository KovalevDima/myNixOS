{pkgs}:

let compiler = pkgs.haskellPackages.callCabal2nix "compiler" ./. {};
in

pkgs.stdenv.mkDerivation {
  name = "personal-page";

  src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;

  buildPhase = "${pkgs.lib.getExe' compiler "compiler"} build --verbose";

  installPhase = ''
    mkdir -p "$out"
    cp -r ./_site "$out"
  '';
}
