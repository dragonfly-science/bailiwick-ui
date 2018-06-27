let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "9cd56b0c56b3f45470b6369d6b80320c49dc8cd7";
      sha256 = "00qv2ripq2k38jnal3r7wi7jsczi9bcpps6pjqxkw3qdnx0hwxqn";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
  jsaddle-github = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "7eb50cb73a7cbc31ec16916f85a3a89164b4908b";
    sha256 = "1dq99q12ibvsm6jz35jxmcv154n0jcb0k8lfhnx5c28ckgk3g8q7";
  };
  ghcjs-dom-github = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "d17a8078b05e7b06dc2ad5553016181c20bd2f83";
    sha256 = "10g2gf5vdlmnchgy72cdnj1nl4av2dcrm8is8a7q4vp73lc6k7h9";
  };
  reflex-dom-github = nixpkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-dom";
    rev = "a3ae1ee151aba7842fb675d11fb38b5e8f59a0e5";
    sha256 = "1ph0k0rvx92i9z425xm35rwd55m78z56q2fnagshk576hl0k4fjr";
  };
  servant-auth-github = nixpkgs.fetchFromGitHub {
    owner = "hamishmack";
    repo = "servant-auth";
    rev = "77f246501cb5e83074e96a0ce419b58974406173";
    sha256 = "0g1ig082bxa7lh0yh30rbww23cmvww0al44ld3gckn0lidn3ksa2";
  };
in reflex-platform.project ({ pkgs, ... }: {
      packages = {
        bailiwick = ./.;
        jsaddle = "${jsaddle-github}/jsaddle";
        jsaddle-warp = "${jsaddle-github}/jsaddle-warp";
        jsaddle-wkwebview = "${jsaddle-github}/jsaddle-wkwebview";
        jsaddle-dom = pkgs.fetchFromGitHub {
          owner = "ghcjs";
          repo = "jsaddle-dom";
          rev = "5eb4b59bc1aff4066e454ff874481829f505e101";
          sha256 = "09szzwncy0ddckdssxfj1nkv1hf4afnghabmrhhzgy6ksqfq7vdq";
        };
        ghcjs-dom-jsffi = "${ghcjs-dom-github}/ghcjs-dom-jsffi";
        ghcjs-dom-jsaddle = "${ghcjs-dom-github}/ghcjs-dom-jsaddle";
        reflex-dom-contrib = pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "707450daa582c937291a30a56c0d2ece0cfd3037";
          sha256 = "1nqplvr6qvdpih80wnxy46acx8nqpdfjvn8kvsq6vxnsp5gqp8rx";
        };
        servant-reflex = pkgs.fetchFromGitHub {
          owner = "imalsogreg";
          repo = "servant-reflex";
          rev = "1761f87e859f6e77335911fd73fefd4e855f4865";
          sha256 = "14v4ygb5kraikbs429df8vizq7hhr79gg84pqr7ccays137znd2n";
        };
        servant-auth = "${servant-auth-github}/servant-auth";
        chrome-remote-interface-haskell = pkgs.fetchFromGitHub {
          owner = "ThomasCrevoisier";
          repo = "chrome-remote-interface-haskell";
          rev = "106aad97aebf0905a40bdec9effea183c08e2863";
          sha256 = "0di4w3j9kkc82adxc2jpjzkm6gh9qj61i56lpwmnnpl722my25mx";
        };
      };

      overrides = self: super: {
        ghcjs-dom-jsffi = self.callPackage "${ghcjs-dom-github}/ghcjs-dom-jsffi" {};
        ghcjs-dom-jsaddle = pkgs.haskell.lib.dontHaddock (self.callPackage "${ghcjs-dom-github}/ghcjs-dom-jsaddle" {});
        # ghcjs-dom = pkgs.haskell.lib.appendConfigureFlag (self.callPackage "${ghcjs-dom-github}/ghcjs-dom" {}) "-fdebug";
        ghcjs-dom = self.callPackage "${ghcjs-dom-github}/ghcjs-dom" {};
        servant-reflex = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.servant-reflex);
        jsaddle-warp = pkgs.haskell.lib.dontCheck super.jsaddle-warp;
        # self.callPackage ../../haskell/aspen/focus/reflex-platform/jsaddle/jsaddle-wkwebview {};
        # jsaddle-devtools = self.callPackage ../../haskell/aspen/focus/reflex-platform/jsaddle/jsaddle-devtools {};
        reflex-dom-core = self.callPackage "${reflex-dom-github}/reflex-dom-core" {};
        reflex-dom = null;
      };

      shells = {
        ghc = ["bailiwick"];
        ghcjs = ["bailiwick"];
      };
  })