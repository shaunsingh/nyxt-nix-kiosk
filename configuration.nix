{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {

  imports = [
    ./apple-silicon-support  # asahi support
    ./nyxt4-gamescope        # nyxt kiosk under gamescope
    # ./sway                   # sway for when nyxt doesn't work
  ];

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


  # default user config
  users.users = {
    shaurizard = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "video"
        "audio"
        "realtime"
      ];
    };
  };

  # backup kde
  services = {
    desktopManager.plasma6.enable = true;
    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };
  };

  # state
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  networking.hostName = "shaurizard";
  system.stateVersion = "23.05";
}
