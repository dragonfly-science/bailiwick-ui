with import ./.;

let
  inherit (reflex) nixpkgs;
  inherit (nixpkgs) lib;

  bailiwick-static = nixpkgs.runCommand "bailiwick-static" {} ''
    mkdir -p $out/static
    cp -r ${./static}/* $out/static
    chmod +w $out/static
    ${nixpkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=${ghcjs.bailiwick}/bin/bailiwick.jsexe/all.js.externs ${ghcjs.bailiwick}/bin/bailiwick.jsexe/all.js   > $out/static/min.js
  '';
in bailiwick-static

