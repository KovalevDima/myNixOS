{pkgs, graphics}:

let
  glslangValidator = pkgs.lib.getExe' pkgs.glslang "glslangValidator";

  icdPath = pkgs.mesa + /share/vulkan/icd.d/lvp_icd.x86_64.json;
in

pkgs.stdenv.mkDerivation {
  name = "Image of julia fractal";

  src = pkgs.nix-gitignore.gitignoreSourcePure [] ./.;

  buildInputs = [ pkgs.mesa ];

  buildPhase = ''
    ${glslangValidator} -S comp -V ${./julia.glsl} -o shader.spirv
    VK_DRIVER_FILES="${icdPath}" ${pkgs.lib.getExe' graphics "graphics"} shader.spirv
  '';

  installPhase = ''
    mkdir -p "$out"
    cp ./result.png "$out"
  '';
}
