with import ./.;

let
  inherit (reflex) nixpkgs;
  inherit (nixpkgs) lib;

  bailiwick-static = nixpkgs.runCommand "bailiwick-static" {} ''
    mkdir -p $out/static
    cp -r ${./static}/* $out/static
    chmod +w $out/static
    ${nixpkgs.closurecompiler}/bin/closure-compiler ${ghcjs.bailiwick}/bin/bailiwick.jsexe/all.js > $out/static/min.js
    ${nixpkgs.closurecompiler}/bin/closure-compiler $out/static/bailiwick.js > $out/static/bailiwick.min.js
  '';
in bailiwick-static

