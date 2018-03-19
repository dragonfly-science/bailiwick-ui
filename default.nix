let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "55bddde319d7c8f6edcc80941926c611098f0875";
      sha256 = "0680vapmfwbj308mcg4fgr60zjb7j6in2p3lppzsd7ydlfhnsbvv";
    }) {};
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
