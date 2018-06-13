let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "f003577699ad5a47f8275dad4f05cdb15c4bcdf5";
      sha256 = "1fwg9cfz6p6zrlk1j5648r9hc5s2m62cwwv036sc7byb3pdhlxdr";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
  servant-auth-github = nixpkgs.fetchFromGitHub {
    owner = "hamishmack";
    repo = "servant-auth";
    rev = "77f246501cb5e83074e96a0ce419b58974406173";
    sha256 = "0g1ig082bxa7lh0yh30rbww23cmvww0al44ld3gckn0lidn3ksa2";
  };
in reflex-platform.project ({ pkgs, ... }: {
      packages = {
        bailiwick = ./.;
        reflex-dom-contrib = pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
          sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
        };
        servant-reflex = pkgs.fetchFromGitHub {
          owner = "imalsogreg";
          repo = "servant-reflex";
          rev = "6593c1ce806a2ac247fa786184c906616236dbd7";
          sha256 = "0sv5qd51ia8zav7aqpmz7v0y34kimi4f4ngqql5b3sl6qqn3zq7q";
        };
        servant-auth = "${servant-auth-github}/servant-auth";
      };

      overrides = self: super: {
        servant-reflex = pkgs.haskell.lib.doJailbreak super.servant-reflex;
        jsaddle-wkwebview = null;
      };

      shells = {
        ghc = ["bailiwick"];
        ghcjs = ["bailiwick"];
      };
  })