{pkgs}:

pkgs.haskell.lib.addBuildDepends
  (pkgs.haskellPackages.callCabal2nix "graphics" ./. {})
  [pkgs.glslang pkgs.vulkan-headers pkgs.vulkan-loader]
