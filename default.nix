let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "ca2dc8a7768abbcdf3edd3ede9c359144c84dd3f";
      sha256 = "0zqg9fq7bnl1zr673ij73cd0z95w38qp9i1r7gjc1f5zi8gmpwhx";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
in reflex-platform.project ({ pkgs, ... }: {
      packages = {
        bailiwick = ./.;
        reflex-dom-contrib = pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "fbd2bc4279146a239342147dbe9f1b9264c63ceb";
          sha256 = "04j06iaabk0ajdi19qv588ybd29wdm0scbnyyfnb9p010a4pwm3f";
        };
##        servant-reflex = pkgs.fetchFromGitHub {
##          owner = "hamishmack";
##          repo = "servant-reflex";
##          rev = "37461d27c8ff31a8876cee8b4beb5492c606f2a0";
##          sha256 = "14v4ygb5kraikbs429df8vizq7hhr79gg84pqr7ccays137znd2n";
##        };
      };

      overrides = self: super:
        let dontCheckGhcjs = p: if self.ghc.isGhcjs or false
                 then pkgs.haskell.lib.dontCheck p
                 else p;
        in {
          servant-reflex = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.servant-reflex);
          reflex-dom = null;
          servant = dontCheckGhcjs super.servant;
          reflex-dom-contrib = pkgs.haskell.lib.doJailbreak super.reflex-dom-contrib;
##          http-date = dontCheckGhcjs super.http-date;
##          simple-sendfile =
##            if self.ghc.isGhcjs or false
##            then pkgs.haskell.lib.overrideCabal (
##                  super.simple-sendfile.overrideAttrs (oldAttr: {
##                      postUnpack = ''
##                      sed -i 's/os(linux)/os(linux) \&\& !impl(ghcjs)/' $sourceRoot/simple-sendfile.cabal
##                  '';
##                  })) (drv: {
##                      libraryHaskellDepends =
##                          drv.libraryHaskellDepends ++ [
##                              self.conduit self.conduit-extra self.resourcet ];
##                       })
##            else super.simple-sendfile;
##          iproute = dontCheckGhcjs super.iproute;
##          unix-time = dontCheckGhcjs super.unix-time;
##          silently = dontCheckGhcjs super.silently;
##          mockery = dontCheckGhcjs super.mockery;
##          Glob = dontCheckGhcjs super.Glob;
##          http2 = dontCheckGhcjs super.http2;
##          bsb-http-chunked = dontCheckGhcjs super.bsb-http-chunked;
##          SHA = dontCheckGhcjs super.SHA;
##          unliftio = dontCheckGhcjs super.unliftio;
##          wai-extra = dontCheckGhcjs super.wai-extra;
##          conduit = dontCheckGhcjs super.conduit;
##          warp = if self.ghc.isGhcjs or false
##            then super.warp.overrideAttrs (_: {
##                configureFlags = ["-f-allow-sendfilefd"];
##              })
##            else super.warp;
##          wai-app-static = dontCheckGhcjs super.wai-app-static;
      };

      shells = {
        ghc = ["bailiwick"];
        ghcjs = ["bailiwick"];
      };
  })
