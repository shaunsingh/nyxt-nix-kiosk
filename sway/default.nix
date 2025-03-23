{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home-manager.users.shaurizard = import ./home.nix;
#   services.greetd = {
#     enable = true;   
#     settings = {
#       default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd 'sway'";
#       internal_session = {
#         command = "sway";
#         user = "shaurizard";
#       };
#     };   
#   };
}
