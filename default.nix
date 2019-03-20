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
      };
      overrides = self: super:
        let dontCheckGhcjs = p: if self.ghc.isGhcjs or false
                 then pkgs.haskell.lib.dontCheck p
                 else p;
        in {
          servant = dontCheckGhcjs super.servant;
          servant-reflex = pkgs.haskell.lib.dontCheck
                 (pkgs.haskell.lib.doJailbreak super.servant-reflex);
          reflex-dom = null;
          reflex-dom-contrib = pkgs.haskell.lib.doJailbreak super.reflex-dom-contrib;
      };

      shells = {
        ghc = ["bailiwick"];
        ghcjs = ["bailiwick"];
      };
  })
