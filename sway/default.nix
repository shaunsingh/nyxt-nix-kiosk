{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home-manager.users.nyxtkiosk = import ./home.nix;
  environment.sessionVariables.WLR_RENDERER = "vulkan";
}
