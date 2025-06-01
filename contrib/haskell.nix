{ pkgs
, ghc
}:
{
  autoWire = ["packages" "apps"];
  basePackages = pkgs.haskell.packages.${ghc};
  settings = {
    graphics = {libraryProfiling = true; executableProfiling = true;};
    oidc-client = {
      broken = false;
      jailbreak = true;
    };
  };
}
