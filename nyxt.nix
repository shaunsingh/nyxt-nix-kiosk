{ config, lib, pkgs, ... }:

let
  nyxt-gamescope = pkgs.writeShellScriptBin "nyxt-gamescope" ''
    ${pkgs.gamescope}/bin/gamescope --fullscreen -- ${nyxt4}/bin/nyxt "$@"
  '';
in
{
  nixpkgs.packages = lib.attrValues {
    inherit
      (pkgs)
      nyxt-gamescope
      # dev
      zola
      ;
    inherit
      (pkgs.python3Packages)
      grip 
      ;
  };
  fonts = {
    packages = builtins.attrValues {
      inherit (pkgs)
        sf-mono-liga-bin
        otf-apple
        ;
    };
    fontconfig = {
      enable = lib.mkDefault true;
      antialias = true;
      subpixel.lcdfilter = "default";
    };
  };
  xdg.configFile."nyxt" = {
    source = ./config;
    recursive = true;
  };
}