{ ... }:

{
  imports = [
    ./modules/default.nix
    ./hardware-configuration.nix
  ];

  # default is impure, use local firmware
  hardware.asahi.peripheralFirmwareDirectory = ./firmware;

  # other asahi options
  hardware.asahi.withRust = true;
  hardware.asahi.useExperimentalGPUDriver = true;

  # memswap
  zramSwap = {
    enable = true;
    memoryPercent = 40;
  };

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
}
