{pkgs}:

let
  glslangValidator = pkgs.lib.getExe' pkgs.glslang "glslangValidator";

  graphics =
    pkgs.haskell.lib.addBuildDepends
      (pkgs.haskellPackages.callCabal2nix "graphics" ./. {})
      (with pkgs; [glslang vulkan-headers vulkan-loader]);
  shaderRunner = pkgs.lib.getExe' graphics "graphics";

  icdPath = pkgs.mesa.drivers + /share/vulkan/icd.d/lvp_icd.x86_64.json;
in

pkgs.stdenv.mkDerivation {
  name = "Image of julia fractal";

  src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;

  buildInputs = [ pkgs.mesa.drivers ];

  buildPhase = ''
    ${glslangValidator} -S comp -V ${./julia.glsl} -o shader.spirv
    VK_DRIVER_FILES="${icdPath}" ${shaderRunner} shader.spirv
  '';

  installPhase = ''
    mkdir -p "$out"
    cp ./result.png "$out"
  '';
}
