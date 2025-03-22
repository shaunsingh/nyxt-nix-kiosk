{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {

  imports = [
    ./hardware-configuration.nix # hardware scan
    ./apple-silicon-support      # asahi support
    ./kde                        # base kde install to develop with
    #./nyxt                       # nyxt kiosk under gamescope
  ];

  # recommended for asahi
  boot = {
    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = false;
    };
    # For ` to < and ~ to > (for those with US keyboards)
    extraModprobeConfig = ''
      options hid_apple iso_layout=0
    '';.
  };

  # wpa_supplicant + wpa3 iffy
  networking.wireless.iwd = {
    enable = true;
    settings.General.EnableNetworkConfiguration = true;
  };

  # memswap
  zramSwap = {
    enable = true;
    memoryPercent = 40;
  };

  # enable flakes + nix-command
  nix = let
    flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
  in {
    settings = {
      experimental-features = "nix-command flakes";
      flake-registry = "";
    };
    channel.enable = false;
    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
    nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
  };

  # allow using proprietary packages
  nixpkgs.config.allowUnfree = true;

  # default user config
  users.users = {
    shaurizard = {
      isNormalUser = true;
      extraGroups = ["wheel"];
    };
  };

  displayManager = {
     sddm = {
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