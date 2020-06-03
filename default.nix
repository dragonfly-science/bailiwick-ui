let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "c13cb19f49c8093de4718d2aced1930128476cfa";
      sha256 = "0v87ilal9355xwz8y9m0zh14pm9c0f7pqch0854kkj92ybc5l62q";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
  cleanSource = nixpkgs.lib.cleanSourceWith {
      src = ./.;
      filter = path: type:
        nixpkgs.lib.all (i: toString i != path) [
          # These are .gitignored sow we should exclude them here
            ./dist
            ./dist-ghcjs
            ./dist-newstyle
            ./db/dev
            ./node_modules
        ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [
            "result"
            ".env"
            "bailiwick.bundle.js.map"
            "Dockerfile"
        ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i (baseNameOf path))) [
            "nix-serve."
            ".ghc.environment."
        ];
  };
in reflex-platform.project ({ pkgs, ... }: {
      packages = {
        bailiwick = cleanSource;
        reflex-dom-contrib = pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
          sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
        };
        servant-reflex = pkgs.fetchFromGitHub {
          owner = "imalsogreg";
          repo = "servant-reflex";
          rev = "37a3e8f2566627d910df140982bd49bf4dba171e";
          sha256 = "1yqxf6f81n4y4527rl69hfqymrnmj7lskgns2qsh59ibisp3y9rg";
        };
      };

      shells = {
        ghc = ["bailiwick"];
        ghcjs = ["bailiwick"];
      };
  })
