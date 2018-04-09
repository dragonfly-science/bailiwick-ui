let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "55bddde319d7c8f6edcc80941926c611098f0875";
      sha256 = "0680vapmfwbj308mcg4fgr60zjb7j6in2p3lppzsd7ydlfhnsbvv";
    }) {};
  servant-auth-github = nixpkgs.fetchFromGitHub {
    owner = "hamishmack";
    repo = "servant-auth";
    rev = "77f246501cb5e83074e96a0ce419b58974406173";
    sha256 = "0g1ig082bxa7lh0yh30rbww23cmvww0al44ld3gckn0lidn3ksa2";
  };
  nixpkgs = reflex-platform.nixpkgs;
  # Work around bug in slightly old nixpkgs.writeShellScriptBin used by reflex-platform
  writeShellScriptBin = name : text :
    nixpkgs.writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";
      text = ''
        #!${nixpkgs.stdenv.shell}
        ${text}
        '';
      checkPhase = ''
        ${nixpkgs.stdenv.shell} -n $out/bin/${name}
      '';
    };
  project = reflex-platform.project ({ pkgs, ... }: {
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
        reflex-dom = null; # Work around issue where servant-reflex added webkitgtk dependency to server executable
      };

      shells = {
        ghc = ["bailiwick"];
        ghcjs = ["bailiwick"];
      };
  });

  shells = project.shells;

in project // {
  inherit shells writeShellScriptBin;
}
