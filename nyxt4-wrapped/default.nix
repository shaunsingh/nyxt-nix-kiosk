{ config, lib, pkgs, ... }:

let 
  # needed fonts
  otf-apple = pkgs.callPackage ./otf-apple.nix { };
  sf-mono-liga-bin = pkgs.callPackage ./sf-mono-liga-bin.nix { };

  # nyxt wrappers
  nyxt4 = pkgs.nyxt.overrideAttrs (oldAttrs: rec {
    pname = "nyxt4";
    version = "4.0.0-pre-release-3";
    src = pkgs.fetchzip {
      url = "https://github.com/atlas-engineer/nyxt/releases/download/${version}/nyxt-${version}-source-with-submodules.tar.xz";
      hash = "sha256-T5p3OaWp28rny81ggdE9iXffmuh6wt6XSuteTOT8FLI=";
      stripRoot = false;
    };
  });
in {
  options.nyxt4-wrapped = {
    display = lib.mkOption {
      type = lib.types.str;
      description = "display output to use";
    };
    resolution = lib.mkOption {
      type = lib.types.str;
      description = "default resolution";
    };
    scale = lib.mkOption {
      type = lib.types.int;
      description = "integer scale for gui";
    };
  };
  config = {
    # vulkan issues
    programs.gamescope = {
      enable = true;
      capSysNice = true;
    };
  
    environment.defaultPackages = with pkgs; [
      # apps
      nyxt4
      cage

      # tools
      wlr-randr
      grim
      slurp
  
      # wrapped
      (pkgs.writeShellScriptBin "nyxt-gamescope" ''
        gamescope -f -s ${builtins.toString config.nyxt4-wrapped.scale} -- nyxt "$@"
      '')
      (pkgs.writeShellScriptBin "nyxt-cage" ''
        GTK_USE_PORTAL=0 cage -m last -s -d -- sh -c 'wlr-randr --output ${config.nyxt4-wrapped.display} --mode ${config.nyxt4-wrapped.resolution} --scale ${builtins.toString config.nyxt4-wrapped.scale} && nyxt'
      '')
 
      # dev dependencies 
      zola 
    ] ++ (with pkgs.python3Packages; [
      grip
    ]);
  
    fonts = {
      packages = with pkgs; [
        sf-mono-liga-bin
        otf-apple
        twemoji-color-font
        sarasa-gothic
      ];
      fontconfig = {
        enable = lib.mkDefault true;
        antialias = true;
        subpixel.lcdfilter = "default";
      };
    };
  
    # Configure XDG for Nyxt
    # xdg = {
    #   enable = true;
    #   configFile."nyxt" = {
    #     source = ./config;
    #     recursive = true;
    #     target = ".config/nyxt";
    #   };
    # };
  };
}
