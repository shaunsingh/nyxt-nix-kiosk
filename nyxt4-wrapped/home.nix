{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  xdg.configFile."nyxt/config.lisp".source = ./config.lisp;
}
