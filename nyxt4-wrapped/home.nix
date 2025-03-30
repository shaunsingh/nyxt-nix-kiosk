{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  xdg.configFile."nyxt" = {
    source = ../config;
    recursive = true;
  };
}
