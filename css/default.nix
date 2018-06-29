{ pkgs ?
    # Default for CI reproducibility, optionally override in your configuration.nix.
    (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      rev = "4b649a99d8461c980e7028a693387dc48033c1f7";
      sha256 = "0iy2gllj457052wkp20baigb2bnal9nhyai0z9hvjr3x25ngck4y";
    }) {})
, mkDerivation ? pkgs.stdenv.mkDerivation
, sass ? pkgs.sass }:

let nodePackages = import ./node2nix { inherit pkgs; };

in mkDerivation {
  name = "dragonfly-1.0.0";
  src =
    builtins.filterSource (path: type:
      pkgs.lib.all (i: toString i != path) [ ./default.nix ]
        && pkgs.lib.all (i: i != baseNameOf path) [ ".git" ".DS_Store" "result" ]
      ) ./.;
  buildInputs = [];
  buildTools = [ sass nodePackages.postcss-cli nodePackages.font-awesome ];
  buildPhase = ''
    mkdir $out
    export LC_ALL="en_US.UTF-8"
    export LANG="en_US.UTF-8"
    export NODE_PATH=${nodePackages.autoprefixer}/lib/node_modules
    ${sass}/bin/sass -I ${nodePackages.font-awesome}/lib/node_modules -t compact app.scss > $out/bailiwick-pre.css
    ${nodePackages.postcss-cli}/bin/postcss --use autoprefixer -o $out/bailiwick.css $out/bailiwick-pre.css
  '';

  installPhase = ":";
}

