let
  pkgs = import <nixpkgs> {};
in with pkgs;
{
  bailiwick-data = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      ggplot2
      data_table
      jsonlite
      yaml
      ];
  };
}
