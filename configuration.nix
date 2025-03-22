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
    '';
  };

  # default is impure, use local firmware
  hardware.asahi.peripheralFirmwareDirectory = ./firmware;

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

  # allow using proprietary packages + install essentials
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    vim  
    git
    wget
    chromium
  ];

  # backup kde
  services = {
    desktopManager.plasma6.enable = true;
    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };
  };

  # default user config
  users.users = {
    shaurizard = {
      isNormalUser = true;
      extraGroups = ["wheel"];
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
