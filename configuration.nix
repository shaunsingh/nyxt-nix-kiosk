{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {

  imports = [
    ./apple-silicon-support  # asahi support
    ./nyxt4-wrapped          # nyxt kiosk
    ./sway                   # sway for when nyxt doesn't work
  ];

  # configuration for nyxt kiosk
  nyxt4-wrapped = {
    display = "eDP-1";
    resolution = "2560x1600";
    scale = 2;
  };

  # theme our console
  console =
    let
      normal = [ "161616" "33b1ff" "ff7eb6" "42be65" "08bdba" "82cfff" "78a9ff" "dde1e6" ];
      bright = [ "525252" "33b1ff" "ff7eb6" "42be65" "08bdba" "82cfff" "78a9ff" "ffffff" ];
    in
    {
      colors = normal ++ bright;
      keyMap = "us";
    };

  # essentials + testing
  environment.systemPackages = with pkgs; [
    vim
    git
    wget
    chromium
    vscode-fhs
    mesa-demos
    vulkan-tools
  ];

  # graphics
  hardware.graphics.enable = true;

  # default user config
  users.users = {
    nyxtkiosk = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "video"
        "audio"
        "realtime"
      ];
    };
  };

  # log us in & launches enviornment of choice automatically
  services.getty.autologinUser = "nyxtkiosk";
  systemd.user.services.nyxtkiosk1 = {
    description = "launch kiosk";
    serviceConfig.PassEnvironment = "DISPLAY";
    script = ''
      nyxt-cage
    '';
    wantedBy = [ "multi-user.target" ];
  };

  # state
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  networking.hostName = "nyxtkiosk";
  system.stateVersion = "23.05";
}
