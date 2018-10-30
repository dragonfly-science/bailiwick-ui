let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "7e002c573a3d7d3224eb2154ae55fc898e67d211";
      sha256 = "1adhzvw32zahybwd6hn1fmqm0ky2x252mshscgq2g1qlks915436";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
  servant-auth-github = nixpkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant-auth";
    rev = "dde682a3fd49bf27b2f94755a5987b7ff4f64bfb";
    sha256 = "1pd8h5b20i8drf6y1k0fjvl97n87rhkma82pxynnwq5b90iqy0mj";
  };
in reflex-platform.project ({ pkgs, ... }: {
      packages = {
        bailiwick = ./.;
        reflex-dom-contrib = pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "9900f2d433240a3f93cdae930a6ffbb73c50bb86";
          sha256 = "1z8cnnhibsiap08pq2iw1r5zqvbla6hci7dhrz9mhfr0nqyryk65";
        };
        servant-reflex = pkgs.fetchFromGitHub {
          owner = "hamishmack";
          repo = "servant-reflex";
          rev = "37461d27c8ff31a8876cee8b4beb5492c606f2a0";
          sha256 = "14v4ygb5kraikbs429df8vizq7hhr79gg84pqr7ccays137znd2n";
        };
        servant-auth = "${servant-auth-github}/servant-auth";
        servant-auth-server = "${servant-auth-github}/servant-auth-server";
        chrome-remote-interface-haskell = pkgs.fetchFromGitHub {
          owner = "ThomasCrevoisier";
          repo = "chrome-remote-interface-haskell";
          rev = "106aad97aebf0905a40bdec9effea183c08e2863";
          sha256 = "0di4w3j9kkc82adxc2jpjzkm6gh9qj61i56lpwmnnpl722my25mx";
        };
        memory = pkgs.fetchFromGitHub {
          owner = "vincenthz";
          repo = "hs-memory";
          rev = "a658506d2d42ddcc7fd5cab4110f7a82852edb0b";
          sha256 = "0b7a29cssmp77mqi9qxcarc9nsijpja75pn3afhyqjfh235q6l5s";
        };
      };

      overrides = self: super:
       let dontCheckGhcjs = p: if self.ghc.isGhcjs or false
                then pkgs.haskell.lib.dontCheck p
                else p;
       in {
        servant-reflex = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.servant-reflex);
        reflex-dom = null;
        servant = dontCheckGhcjs super.servant;
        servant-auth = pkgs.haskell.lib.doJailbreak super.servant-auth;
        http-date = dontCheckGhcjs super.http-date;
        simple-sendfile = if self.ghc.isGhcjs or false
          then pkgs.haskell.lib.overrideCabal (super.simple-sendfile.overrideAttrs (oldAttr: {
              postUnpack = ''
                sed -i 's/os(linux)/os(linux) \&\& !impl(ghcjs)/' $sourceRoot/simple-sendfile.cabal
              '';
            })) (drv: {
              libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.conduit self.conduit-extra self.resourcet ];
            })
          else super.simple-sendfile;
        memory = dontCheckGhcjs super.memory;
        iproute = dontCheckGhcjs super.iproute;
        unix-time = dontCheckGhcjs super.unix-time;
        silently = dontCheckGhcjs super.silently;
        Glob = dontCheckGhcjs super.Glob;
        http2 = dontCheckGhcjs super.http2;
        bsb-http-chunked = dontCheckGhcjs super.bsb-http-chunked;
        SHA = dontCheckGhcjs super.SHA;
        warp = if self.ghc.isGhcjs or false
          then super.warp.overrideAttrs (_: {
              configureFlags = ["-f-allow-sendfilefd"];
            })
          else super.warp;
        wai-app-static = dontCheckGhcjs super.wai-app-static;
      };

      shells = {
        ghc = ["bailiwick" "reflex-dom-contrib"];
        ghcjs = ["bailiwick" "reflex-dom-contrib"];
      };
  })
