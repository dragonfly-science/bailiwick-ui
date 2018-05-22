with import ./.;

let
  inherit (reflex) nixpkgs;
  inherit (nixpkgs) lib;

  bailiwick-static = nixpkgs.runCommand "bailiwick-static" {} ''
    mkdir -p $out/static
    cp -r ${./static}/* $out/static
    chmod +w $out/static
    ${nixpkgs.closurecompiler}/bin/closure-compiler ${ghcjs.bailiwick}/bin/bailiwick.jsexe/all.js > $out/static/min.js
    ${ghc.bailiwick}/bin/make-map-css $out/static/map.css.tmpl $out/static/data/areas-1b7549470.json > $out/static/map.css
  '';
in bailiwick-static

