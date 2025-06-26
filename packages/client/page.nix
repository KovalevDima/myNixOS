{pkgs}:

pkgs.buildNpmPackage {
  name = "@monorepo/hello";

  version = "1.0.0";

  src = ./../..;

  npmDepsHash = "sha256-hF35THrzK6/EcF/5OR29xalbBysDdOLNenBZJ/S2qjQ=";
  # npmDepsHash = "${pkgs.lib.fakeHash}";

  # src = pkgs.nix-gitignore.gitignoreSourcePure [ "page.nix" ] ./.;

  npmBuild = "npm run build";

  installPhase = ''
    mkdir --parents $out
    ls
    cp --archive ./packages/client/dist/. $out
  '';
}
