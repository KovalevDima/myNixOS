{pkgs, nixpkgs, graphics}:

pkgs.writeShellScriptBin "mkImage" ''
  glslangValidator -S comp -V ${./julia.glsl} -o ./~compiledShader.spirv
  ${nixpkgs.lib.getExe' graphics "graphics"} ./~compiledShader.spirv
  rm ./~compiledShader.spirv
''
