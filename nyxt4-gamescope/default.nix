{ config, lib, pkgs, ... }:

let 
  otf-apple = pkgs.callPackage ./otf-apple.nix { };
  sf-mono-liga-bin = pkgs.callPackage ./sf-mono-liga-bin.nix { };
  nyxt4 = pkgs.nyxt.overrideAttrs (oldAttrs: rec {
    pname = "nyxt4";
    version = "4.0.0-pre-release-3";
    src = pkgs.fetchzip {
      url = "https://github.com/atlas-engineer/nyxt/releases/download/${version}/nyxt-${version}-source-with-submodules.tar.xz";
      hash = "sha256-T5p3OaWp28rny81ggdE9iXffmuh6wt6XSuteTOT8FLI=";
      stripRoot = false;
    };
  });
  nyxt-wrapped = pkgs.writeShellScriptBin "nyxt-wrapped" ''
    ${nyxt4}/bin/nyxt & ${pkgs.ttyd}/bin/ttyd -t rendererType=canvas -t 'theme={"background": "#161616", "foreground": "#ffffff", "cursor": "#f2f4f8", "black": "#161616", "red": "#78a9ff", "green": "#ff7eb6", "yellow": "#42be65", "blue": "#08bdba", "magenta": "#82cfff", "cyan": "#33b1ff", "white": "#f2f4f8", "brightBlack": "#c1c7cd", "brightRed": "#78a9ff", "brightGreen": "#ff7eb6", "brightYellow": "#42be65", "brightBlue": "#08bdba", "brightMagenta": "#82cfff", "brightCyan": "#33b1ff", "brightWhite": "#ffffff"}' -t disableLeaveAlert=true -t disableResizeOverlay=true  -t enableSixel=true -t fontSize=15 fish
  '';
  nyxt-gamescope = pkgs.writeShellScriptBin "nyxt-gamescope" ''
    ${pkgs.gamescope}/bin/gamescope --fullscreen -- ${nyxt-wrapped}/bin/nyxt "$@"
  '';
  nyxt-cage = pkgs.writeShellScriptBin "nyxt-cage" ''
    ${pkgs.cage}/bin/cage ${nyxt-wrapped}/bin/nyxt
  '';
in {
  environment.defaultPackages = with pkgs; [
    nyxt4
    nyxt-wrapped
    nyxt-gamescope
    nyxt-cage
    zola 
  ] ++ (with pkgs.python3Packages; [
    grip
  ]);

  fonts = {
    packages = with pkgs; [
      sf-mono-liga-bin
      otf-apple
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

  # vulkan issues
  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };
#  systemd.services.gamescopeService = {
#    wantedBy = [ "graphical.target" ];
#    after = [
#      "systemd-user-sessions.service"
#      "plymouth-start.service"
#      "plymouth-quit.service"
#      "systemd-logind.service"
#      "getty@tty1.service"
#    ];
#    before = [ "graphical.target" ];
#    wants = [
#      "dbus.socket"
#      "systemd-logind.service"
#      "plymouth-quit.service"
#    ];
#    conflicts = [ "getty@tty1.service" ];
#    restartIfChanged = false;
#    unitConfig.ConditionPathExists = "/dev/tty1";
#    serviceConfig = {
#      ExecStart = ''
#        ${nyxt-gamescope}/bin/nyxt-gamescope
#      '';
#      User = "shaurizard";    
#      IgnoreSIGPIPE = "no";
#      # Log this user with utmp, letting it show up with commands 'w' and
#      # 'who'. This is needed since we replace (a)getty.
#      UtmpIdentifier = "%n";
#      UtmpMode = "user";
#      # A virtual terminal is needed.
#      TTYPath = "/dev/tty1";
#      TTYReset = "yes";
#      TTYVHangup = "yes";
#      TTYVTDisallocate = "yes";
#      # Fail to start if not controlling the virtual terminal.
#      StandardInput = "tty-fail";
#      StandardOutput = "journal";
#      StandardError = "journal";
#      # Cage needs a full (custom) user session, might as well let gamescope have it too
#      PAMName = "gamescope";
#    };
#  };
  # services.cage = {
  #   enable = true;
  #   user = "shaurizard";
  #   program = "${nyxt4}/bin/nyxt";
  # };
}
