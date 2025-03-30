{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
/*
home-manager configuration
Useful links:
- Home Manager Manual: https://nix-community.gitlab.io/home-manager/
- Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
let
  eww = inputs.eww.packages.aarch64-linux.eww;
in {
  ### -- home
  home = {
    packages = builtins.attrValues {
      inherit
        (pkgs)
        wayland-utils
        xdg-utils
        grim
        slurp
        swaybg
        swayidle
        swaylock
        wf-recorder
        wl-clipboard
        wlogout
        doublecmd
        ;
    };

    sessionPath = [
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    stateVersion = "23.05";
  };

  ### -- sway
  wayland.windowManager.sway = {
    enable = true;
    # package = pkgs.swayfx-unwrapped;
    systemd.enable = true;
    extraSessionCommands = ''
      export NIXOS_OZONE_WL=1
      export XDG_SESSION_DESKTOP=sway
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORMTHEME=qt5ct
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export CLUTTER_BACKEND=wayland
      export ECORE_EVAS_ENGINE=wayland-egl
      export ELM_ENGINE=wayland_egl
      export NO_AT_BRIDGE=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    wrapperFeatures.gtk = true;
    config = {
      startup = [
        {
          command = "${eww}/bin/eww open bar && ${eww}/bin/eww open bar2";
          always = false;
        }
      ];
      window = {
        titlebar = true;
        border = 0;
      };
      input = {
        "keyboard" = {
          xkb_layout = "us";
          xkb_options = "caps:super";
        };
        "type:mouse" = {
          dwt = "disabled";
          accel_profile = "flat";
        };
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
          accel_profile = "adaptive";
          scroll_factor = "0.45";
          pointer_accel = "0.27";
        };
      };
      output = {
        "*" = {
          background = "#000000 solid_color";
        };
      };
      bars = lib.mkForce [];
      gaps.outer = 18;
      defaultWorkspace = "workspace 1";
      keybindings = let
        modifier = "Mod4";
        concatAttrs = lib.fold (x: y: x // y) {};
        tagBinds =
          concatAttrs
          (map
            (i: {
              "${modifier}+${toString i}" = "exec 'swaymsg workspace ${toString i} && ${eww}/bin/eww update active-tag=${toString i}'";
              "${modifier}+Shift+${toString i}" = "exec 'swaymsg move container to workspace ${toString i}'";
            })
            (lib.range 0 9));
        screenshot = pkgs.writeShellScriptBin "screenshot" ''
          ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - -t png | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png
        '';
        ocrScript = let
          inherit (pkgs) grim libnotify slurp tesseract5 wl-clipboard;
          _ = pkgs.lib.getExe;
        in
          pkgs.writeShellScriptBin "wl-ocr" ''
            ${_ grim} -g "$(${_ slurp})" -t ppm - | ${_ tesseract5} - - | ${wl-clipboard}/bin/wl-copy
            ${_ libnotify} "$(${wl-clipboard}/bin/wl-paste)"
          '';
        volume = pkgs.writeShellScriptBin "volume" ''
          #!/bin/sh

          ${pkgs.pamixer}/bin/pamixer "$@"
          volume="$(${pkgs.pamixer}/bin/pamixer --get-volume)"

          if [ $volume = 0 ]; then
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Volume" \
                  "Muted" \
                  -t 888 \
                  -u low
          else
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Volume" "Currently at $volume%" \
                  -h int:value:"$volume" \
                  -t 888 \
                  -u low
          fi

          ${eww}/bin/eww update volume-level=$volume
        '';
        microphone = pkgs.writeShellScriptBin "microphone" ''
          #!/bin/sh

          ${pkgs.pamixer}/bin/pamixer --default-source "$@"
          mic="$(${pkgs.pamixer}/bin/pamixer --default-source --get-volume-human)"

          if [ "$mic" = "muted" ]; then
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Microphone" \
                  "Muted" \
                  -t 888 \
                  -u low
          else
            ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Microphone" "Currently at $mic" \
                  -h int:value:"$mic" \
                  -t 888 \
                  -u low
          fi
        '';
        brightness = let
          brightnessctl = pkgs.brightnessctl + "/bin/brightnessctl";
        in
          pkgs.writeShellScriptBin "brightness" ''
            #!/bin/sh

            ${brightnessctl} "$@"
            brightness=$(echo $(($(${brightnessctl} g) * 100 / $(${brightnessctl} m))))

            ${pkgs.libnotify}/bin/notify-send -r 69 \
                -a "Brightness" "Currently at $brightness%" \
                -h int:value:"$brightness" \
                -t 888 \
                -u low

            ${eww}/bin/eww update brightness-level=$brightness
          '';
      in
        tagBinds
        // {
          "${modifier}+Return" = "exec ${pkgs.foot}/bin/foot";
          # "${modifier}+d" = "exec ${pkgs.kickoff}/bin/kickoff";
          # "${modifier}+p" = "exec ${screenshot}/bin/screenshot";
          "${modifier}+p" = "exec ${pkgs.grim}/bin/grim";
          "${modifier}+Shift+p" = "exec ${ocrScript}/bin/wl-ocr";
          "${modifier}+v" = "exec ${volume}/bin/volume -d 5";
          "${modifier}+b" = "exec ${volume}/bin/volume -i 5";
          "${modifier}+Shift+v" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "${modifier}+Shift+b" = "exec ${volume}/bin/volume -t";
          "${modifier}+n" = "exec ${brightness}/bin/brightness set 5%-";
          "${modifier}+m" = "exec ${brightness}/bin/brightness set 5%+";
          "${modifier}+Shift+n" = "exec ${pkgs.playerctl}/bin/playerctl previous";
          "${modifier}+Shift+m" = "exec ${pkgs.playerctl}/bin/playerctl next";
          "${modifier}+q" = "kill";
          "${modifier}+r" = ''mode "resize"'';
          "${modifier}+h" = "focus left";
          "${modifier}+j" = "focus down";
          "${modifier}+k" = "focus up";
          "${modifier}+l" = "focus right";
          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+j" = "move down";
          "${modifier}+Shift+k" = "move up";
          "${modifier}+Shift+l" = "move right";
          "${modifier}+s" = "layout stacking";
          "${modifier}+w" = "layout tabbed";
          "${modifier}+e" = "layout toggle split";
          "${modifier}+f" = "fullscreen";
          "${modifier}+space" = "floating toggle";
          "${modifier}+Shift+s" = "sticky toggle";
          "${modifier}+Shift+space" = "focus mode_toggle";
          "${modifier}+a" = "focus parent";
          "${modifier}+Shift+c" = "reload";
          "${modifier}+Shift+e" = "exit";
        };
      colors = {
        focused = {
          background = "#f2f4f8";
          indicator = "#f2f4f8";
          border = "#f2f4f8";
          text = "#f2f4f8";
          childBorder = "#f2f4f8";
        };
        focusedInactive = {
          background = "#262626";
          indicator = "#262626";
          border = "#262626";
          text = "#262626";
          childBorder = "#262626";
        };
        unfocused = {
          background = "#262626";
          indicator = "#262626";
          border = "#262626";
          text = "#262626";
          childBorder = "#262626";
        };
        urgent = {
          background = "#ee5396";
          indicator = "#ee5396";
          border = "#ee5396";
          text = "#ee5396";
          childBorder = "#ee5396";
        };
      };
    };
    extraConfig = ''
      default_border none
      default_floating_border none

      # gestures
      bindgesture swipe:3:right workspace prev
      bindgesture swipe:3:left workspace next
      
      # swayfx
      # shadows enable
    '';
  };
  services.kanshi.systemdTarget = "sway-session.target";

  ### -- launcher
  #   programs.kickoff = {
  #     enable = true;
  #     settings = {
  #       prompt = "λ  ";
  #       padding = 54;
  #       fonts = [ "Liga SFMono Nerd Font" ];
  #       font_size = 21.0;
  #       colors = {
  #         background = "#000000FF";
  #         prompt = "#ff7eb6FF";
  #         text = "#dde1e6FF";
  #         text_query = "#ffffffFF";
  #         text_selected = "#3ddbd9FF";
  #       };
  #     };
  #   };

  ### -- display
  services.kanshi = {
    enable = true;
    profiles = {
      undocked = {
        outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
          }
        ];
      };
    };
  };

  ### -- cursor
  home.pointerCursor = {
    name = "phinger-cursors";
    package = pkgs.phinger-cursors;
  };
  gtk = {
    cursorTheme = {
      name = "phinger-cursors";
      package = pkgs.phinger-cursors;
    };
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };
  home.sessionVariables = {
    XCURSOR_THEME = "phinger-cursors";
    # XCURSOR_SIZE = "32";
  };

  ### -- terminal
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Liga SFMono Nerd Font:size=11";
        pad = "27x27";
        dpi-aware = "no";
        notify = "${pkgs.libnotify}/bin/notify-send -a foot -i foot \${title} \${body}";
      };
      mouse.hide-when-typing = "yes";
      scrollback.lines = 32768;
      url.launch = "${pkgs.xdg-utils}/bin/xdg-open \${url}";
      tweak.grapheme-shaping = "yes";
      cursor.style = "beam";
      colors = {
        background = "161616";
        foreground = "ffffff";
        regular0 = "161616";
        regular1 = "78a9ff";
        regular2 = "ff7eb6";
        regular3 = "42be65";
        regular4 = "08bdba";
        regular5 = "82cfff";
        regular6 = "33b1ff";
        regular7 = "dde1e6";
        bright0 = "525252";
        bright1 = "78a9ff";
        bright2 = "ff7eb6";
        bright3 = "42be65";
        bright4 = "08bdba";
        bright5 = "82cfff";
        bright6 = "33b1ff";
        bright7 = "ffffff";
      };
    };
  };

  ### -- notifications
  services.dunst = {
    enable = true;
    settings = {
      global = {
        # gen settings
        follow = "mouse";
        width = 300;
        origin = "top-left";
        notification_limit = 0;
        offset = "18x18";
        icon_position = "off";
        # progress
        progress_bar_height = 9;
        progress_bar_frame_width = 0;
        # other gen
        padding = 18;
        horizontal_padding = 18;
        frame_width = 0;
        gap_size = 9;
        font = "Liga SFMono Nerd Font 11";
        format = "<span size='x-large' font_desc='Liga SFMono Nerd Font 9' weight='bold' foreground='#dde1e6'>%a</span>\\n%s\\n%b";
        show_indicators = false;
        mouse_left_click = "do_action";
        mouse_middle_click = "close_all";
        mouse_right_click = "close_current";
        ellipsize = "end";
        markup = "full";
      };

      # disable notifs in fullscreen
      fullscreen_delay_everything = {fullscreen = "delay";};

      # colors
      urgency_low = {
        timeout = 3;
        background = "#131313";
        foreground = "#dde1e6";
        highlight = "#be95ff";
      };
      urgency_normal = {
        timeout = 6;
        background = "#131313";
        foreground = "#dde1e6";
        highlight = "#3ddbd9";
      };
      urgency_critical = {
        timeout = 0;
        background = "#131313";
        foreground = "#dde1e6";
        highlight = "#ff7eb6";
      };
    };
  };
  services.poweralertd.enable = true;

  ### -- bar
  programs.eww = {
    enable = true;
    configDir = let
      ewwYuck = pkgs.writeText "eww.yuck" ''
        (defwidget bar []
          (centerbox :orientation "v"
                     :halign "center"
            (box :class "segment-top"
                 :valign "start"
                 :orientation "v"
              (tags))
            (box :class "segment-center"
                 :valign "center"
                 :orientation "v"
              (time)
              (date))
            (box :class "segment-bottom"
                 :valign "end"
                 :orientation "v"
              (menu)
              (brightness)
              (volume)
              (battery)
              (current-tag))))

        (defwidget time []
          (box :class "time"
               :orientation "v"
            hour min type))

        (defwidget date []
          (box :class "date"
               :orientation "v"
            year month day))

        (defwidget menu []
          (button :class "icon"
                  :orientation "v"
                  :onclick "''${EWW_CMD} open --toggle notifications-menu"
             ""))

        (defwidget brightness []
          (button :class "icon"
                  :orientation "v"
            (circular-progress :value brightness-level
                               :thickness 3)))

        (defwidget volume []
          (button :class "icon"
                  :orientation "v"
            (circular-progress :value volume-level
                               :thickness 3)))

        (defwidget battery []
          (button :class "icon"
                  :orientation "v"
                  :onclick ""
            (circular-progress :value "''${EWW_BATTERY['macsmc-battery'].capacity}"
                               :thickness 3)))

        (defwidget current-tag []
          (button :class "current-tag"
                  :orientation "v"
                  :onclick "kickoff & disown"
            "''${active-tag}"))

        (defvar active-tag "1")
        (defpoll hour :interval "1m" "date +%I")
        (defpoll min  :interval "1m" "date +%M")
        (defpoll type :interval "1m" "date +%p")

        (defpoll day   :interval "10m" "date +%d")
        (defpoll month :interval "1h"  "date +%m")
        (defpoll year  :interval "1h"  "date +%y")

        ;; this is updated by the helper script
        (defvar brightness-level 66)
        (defvar volume-level 33)

        ;; sway
        (defwidget tags []
          (box :class "tags"
               :orientation "v"
               :halign "center"
            (for tag in tags
              (box :class {active-tag == tag.tag ? "active" : "inactive"}
                (button :onclick "swaymsg workspace ''${tag.tag} ; ''${EWW_CMD} update active-tag=''${tag.tag}"
                  "''${tag.label}")))))

        (defvar tags '[{ "tag": 1, "label": "一" },
                       { "tag": 2, "label": "二" },
                       { "tag": 3, "label": "三" },
                       { "tag": 4, "label": "四" },
                       { "tag": 5, "label": "五" },
                       { "tag": 6, "label": "六" },
                       { "tag": 7, "label": "七" },
                       { "tag": 8, "label": "八" },
                       { "tag": 9, "label": "九" },
                       { "tag": 0, "label": "" }]')

        (defwindow bar
          :monitor 0
          :stacking "bottom"
          :geometry (geometry
                      :height "100%"
                      :anchor "left center")
          :exclusive true
          (bar))

        (defwindow bar2
          :monitor 1
          :stacking "bottom"
          :geometry (geometry
                      :height "100%"
                      :anchor "left center")
          :exclusive true
          (bar))
      '';

      ewwScss = pkgs.writeText "eww.scss" ''
        $baseTR: rgba(13,13,13,0.13);
        $base00: #131313;
        $base01: #262626;
        $base02: #393939;
        $base03: #525252;
        $base04: #dde1e6;
        $base05: #f2f4f8;
        $base06: #ffffff;
        $base07: #08bdba;
        $base08: #3ddbd9;
        $base09: #33b1ff;
        $base0A: #ee5396;
        $base0B: #78a9ff;
        $base0C: #ff7eb6;
        $base0D: #42be65;
        $base0E: #be95ff;
        $base0F: #82cfff;
        $baseIBM: #0f62fe;

        * {
          all: unset;
        }

        window {
          font-family: "Liga SFMono Nerd Font";
          font-size: 13px;
          background-color: rgba(0,0,0,0);
          color: $base04;
          & > * {
            margin: 0px 0px 12px 12px;
          }
        }

        .tags {
          margin-top: 9px;
        }

        .active {
          color: $base06;
          padding: 6px 9px 6px 6px;
          background-color: $baseTR;
          border-left: 3px solid $base0C;
        }

        .segment-center {
          margin-top: 18px;
          padding: 9px;
        }

        .time {
          color: $base06;
          font-weight: bold;
          margin-bottom: 6px;
        }

        .date {
          margin-top: 6px;
        }

        .icon {
          background-color: $base01;
          padding: 9px;
          margin: 4.5px 0px;
          border-radius: 3px;
        }

        .current-tag {
          color: $base00;
          background-color: $base0E;
          padding: 9px;
          margin: 4.5px 0px;
          border-radius: 3px;
        }
      '';

      ewwConf = pkgs.linkFarm "ewwConf" [
        {
          name = "eww.scss";
          path = ewwScss;
        }
        {
          name = "eww.yuck";
          path = ewwYuck;
        }
      ];
    in
      ewwConf;
  };
  systemd.user.services.eww = {
    Unit = {
      Description = "Eww daemon";
      PartOf = ["graphical-session.target"];
    };
    Service = {
      Environment = let
        dependencies = with pkgs; [
          #kickoff
          brightnessctl
          pamixer
          coreutils
          sway
        ];
      in "PATH=/run/wrappers/bin:${lib.makeBinPath dependencies}";
      ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
      Restart = "on-failure";
    };
    Install.WantedBy = ["graphical-session.target"];
  };

  ### -- sleep
  services.swayidle = let
    display = status: "swaymsg 'output * power ${status}'";
  in {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = display "off";
      }
      {
        event = "before-sleep";
        command = "swaylock";
      }
      {
        event = "after-resume";
        command = display "on";
      }
      {
        event = "lock";
        command = display "off";
      }
      {
        event = "unlock";
        command = display "on";
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = display "off";
        resumeCommand = display "on";
      }
      {
        timeout = 310;
        command = "swaylock";
      }
    ];
  };
}
