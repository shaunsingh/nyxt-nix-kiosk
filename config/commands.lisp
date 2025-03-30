(in-package #:nyxt-user)

;;; SPLIT

#+linux
(define-panel-command-global vsplit 
  (&key (url (quri:render-uri (url (current-buffer)))))
    (panel "*Duplicate panel*" :right)
  "Duplicate the current buffer URL in the panel buffer on the right.

A poor man's vsplit :("
  (setf 
    (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
  (run-thread "URL loader"
    (sleep 0.3)
    (buffer-load (quri:uri url) :buffer panel))
  "")

;;; TRANSLATE

(define-panel-command-global search-translate-selection (&key (selection (ffi-buffer-copy (current-buffer))))
    (panel "*Translate panel*" :right)
  "Open the translation of the selected word in a panel buffer."
  (run-thread "search translation URL loader"
    (setf
      (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
      (sleep 0.3)
      (buffer-load (quri:uri (format nil (nyxt::search-url (nyxt::default-search-engine))
                                     (str:concat "translate " (ffi-buffer-copy (current-buffer)) "to english")))
                    :buffer panel))
   "")
 
(ffi-add-context-menu-command
  'search-translate-selection
  "Translate Selection")

;; LAUNCHER

(defun string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if it is present."
  (if (and suffix 
           string 
           (>= (length string) (length suffix))
           (string= suffix (subseq string (- (length string) (length suffix)))))
      (subseq string 0 (- (length string) (length suffix)))
      string))

;; these are symlinks so might have issues?
(defun get-installed-applications ()
  #+linux
  "Retrieve a list of installed applications from NixOS applications directory."
  (remove-duplicates
   (remove nil
    (mapcar (lambda (file)
              (let ((filename (file-namestring file)))
                (when (and (str:ends-with? ".desktop" filename)
                           (not (str:starts-with? "." filename))
                           (not (string= filename "mimeinfo.cache")))
                  (string-remove-suffix ".desktop" filename))))
            (directory "/run/current-system/sw/share/applications/*.desktop"))
   :test #'string=)))

(defun prompt-application ()
  #+linux
  "Prompt user to select an application to launch."
  (let ((apps (get-installed-applications)))
    (first 
     (prompt 
      :prompt "Select Application to Launch"
      :sources (make-instance 
                'prompter:source 
                :name "Installed Applications"
                :constructor apps)))))

#+linux
(define-command-global launch-application ()
  "Launch a selected application."
  (let ((app (prompt-application)))
    (uiop:launch-program (format nil "gtk-launch ~A" app))
    (echo "Launching application: ~A" app)))

;;; MARKDOWN

(defun my-prompt-for-file ()
  (uiop:native-namestring
   (pathname
    (prompt1
     :prompt "Open file"
     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources
     (list (make-instance 'nyxt/mode/file-manager:file-source
                          :name "Existing file"
                          :actions-on-return #'identity)
           (make-instance 'prompter:raw-source
                          :name "Create new file"))))))

(defun my-prompt-for-directory ()
  "Prompt the user to select a directory."
  (uiop:native-namestring
   (pathname
    (prompt1 
     :prompt "Select Directory"
     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources 
     (list 
      (make-instance 'nyxt/mode/file-manager:file-source
                     :name "Directories"
                     :allow-directories t
                     :path-filter #'uiop:directory-pathname-p))))))

(defun find-zola-config-directory (file-path)
  "Find the nearest parent directory containing config.toml."
  (let ((dir (uiop:pathname-directory-pathname file-path)))
    (loop while dir
          when (probe-file (merge-pathnames "config.toml" dir))
            return dir
          do (setf dir (uiop:pathname-parent-directory-pathname dir)))))

(defun run-zola-serve (directory)
  "Run zola serve in the specified directory."
  (uiop:launch-program 
   (format nil "cd ~a && zola serve -p 1111 --interface 127.0.0.1" 
           (uiop:native-namestring directory))
   :output :interactive
   :error-output :interactive))

(defun kill-zola ()
  "Kill the zola pricess"
  (uiop:launch-program "pkill zola"
   :output :interactive
   :error-output :interactive))

(define-command-global zola-preview ()
  "Preview Zola project in a new buffer"
  (let* ((selected-directory (my-prompt-for-directory))
         (zola-uri (quri:uri "http://localhost:1111")))
      (run-zola-serve selected-directory)
      (make-buffer-focus :url zola-uri)))

#+linux
(define-panel-command-global zola-preview-split () 
  (panel "*zola preview*" :right) 
  "Open the Zola preview of the current markdown file on the right buffer"
  (run-thread "zola preview loader" 
    (setf (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (buffer-load (quri:uri "http://localhost:1111") :buffer panel))
  "")

#+linux
(define-command-global edit-and-preview-with-zola (&key (file (my-prompt-for-file)))
  "Open a markdown file with editor and start Zola preview if possible."
  (let ((buffer (make-instance 'my-editor-buffer 
                               :url (quri:make-uri :scheme "editor" :path file)))
        (zola-dir (find-zola-config-directory file)))
    (set-current-buffer buffer)
    (if zola-dir
        (progn
          (run-zola-serve zola-dir)
          (zola-preview-split)
          (echo "Zola preview started for directory: ~a" zola-dir))
        (echo "No Zola config.toml found in parent directories"))))

(define-command-global zola-kill ()
  "Stop Zola server"
  (kill-zola))

#+linux
(define-command-global close-zola-preview-split ()
  "Close Zola preview window and stop Zola server"
  (delete-all-panel-buffers)
  (kill-zola))

;;; GRIM

#+linux
(define-command-global screenshot ()
  "Take a screenshot with a 2 second delay"
  (uiop:launch-program "sleep 2 && grim"))

#+linux
(define-command-global screenshot-to-clipboard ()
  "Take a screenshot with a 2 second delay & copy to clipboard"
  (uiop:launch-program "sleep 2 && grim - | wl-copy"))

#+linux
(define-command-global screenshot-region ()
  "Take a screenshot of a region and copy to clipboard"
  (uiop:launch-program "grim -g \"$(slurp)\" - -t png | wl-copy -t image/png"))

;;; WF-RECORDER

#+linux
(define-command-global screen-record ()
  "Take a recording of the current display"
  (uiop:launch-program "wf-recorder"))

#+linux
(define-command-global screen-record-region ()
  "Take a recording of a region on the current display"
  (uiop:launch-program "wf-recorder -g \"$(slurp)\""))

;; NMCLI

(defun get-wifi-devices ()
  #+linux
  "Retrieve a list of WiFi devices using nmcli."
  (mapcar 
   #'first 
   (remove-if 
    (lambda (line) 
      (or (null line) 
          (string= (first line) "DEVICE")))
    (mapcar 
     (lambda (line) 
       (str:words line))
     (rest (str:lines (uiop:run-program "nmcli device status" :output :string)))))))

(defun get-available-networks (device)
  #+linux
  "Retrieve available networks for a given WiFi device using nmcli."
  (let* ((wifi-list-output 
           (uiop:run-program 
            (format nil "nmcli -f SSID,BSSID device wifi list ifname ~A" device) 
            :output :string))
         (lines (rest (str:lines wifi-list-output))))
    (let ((seen '()))
      (mapcar
       (lambda (line)
         (let ((parts (str:words line)))
           (let ((ssid (first parts)))
             (unless (or (null ssid) 
                         (string= ssid "") 
                         (string= (str:trim ssid) "--") 
                         (member ssid seen :test 'string=))
               (push ssid seen)
               ssid))))
       (remove-if
        (lambda (line)
          (or (null line)
              (string= line "")
              (string= (str:trim line) "SSID")))
        lines)))))

(defun prompt-wifi-device ()
  #+linux
  "Prompt user to select a WiFi device."
  (let ((devices (get-wifi-devices)))
    (or 
     (first 
      (prompt 
       :prompt "Select WiFi device"
       :sources (make-instance 
                 'prompter:source 
                 :name "WiFi Devices"
                 :constructor devices)))
     (error "No WiFi devices found"))))

(defun prompt-network (device)
  #+linux
  "Prompt user to select a network for the given device."
  (let ((networks (get-available-networks device)))
    (or 
     (first 
      (prompt 
       :prompt "Select Network"
       :sources (make-instance 
                 'prompter:source 
                 :name "Available Networks"
                 :constructor networks))))
     (error "No networks found")))

(defun prompt-wifi-password ()
  #+linux
  "Prompt user to enter WiFi password."
  (first 
   (prompt 
    :prompt "Enter WiFi Password: "
    :sources (make-instance 
              'prompter:raw-source))))

#+linux
(define-command-global connect-wifi ()
  "Connect to a WiFi network using nmcli."
  (let* ((wlan-device (prompt-wifi-device))
         (network-name (prompt-network wlan-device))
         (password (prompt-wifi-password)))
    (let ((command 
            (format nil 
                    "nmcli device wifi connect '~A' password '~A' ifname ~A" 
                    network-name password wlan-device)))
      (uiop:launch-program command)
      (echo "Connecting to ~A on ~A" network-name wlan-device))))

;; BLUETOOTHCTL

(defun get-bluetooth-devices ()
  #+linux
  "Retrieve a list of available Bluetooth devices using bluetoothctl."
  (remove-if #'null
             (mapcar (lambda (line)
                       (when (search "Device" line)
                         (nth 1 (str:words line))))
                     (str:lines (uiop:run-program "bluetoothctl devices" :output :string)))))

(defun prompt-bluetooth-device ()
  #+linux
  "Prompt user to select a Bluetooth device."
  (let ((devices (get-bluetooth-devices)))
    (or 
     (first 
      (prompt 
       :prompt "Select Bluetooth device"
       :sources (make-instance 
                 'prompter:source 
                 :name "Bluetooth Devices"
                 :constructor devices)))
     (error "No Bluetooth devices found"))))

#+linux
(define-command-global connect-bluetooth ()
  "Connect to a Bluetooth device using bluetoothctl."
  (let ((device (prompt-bluetooth-device)))
    (uiop:launch-program (format nil "bluetoothctl connect ~A" device))
    (echo "Connecting to Bluetooth device: ~A" device)))

;;; DFR 

;; utilities

#+linux
(defun get-current-brightness ()
  "Get current brightness as percentage"
  (let* ((current-brightness
           (parse-integer
            (uiop:run-program
             "brightnessctl g"
             :output :string)
            :junk-allowed t)))
    (floor (* 100 (/ current-brightness 100)))))

#+linux
(defun set-brightness-percentage (percentage)
  "Set brightness to given percentage"
  (let* ((clamped-percentage (max 0 (min 100 percentage)))
         (command
           (format nil "brightnessctl s ~A%" clamped-percentage)))
    (uiop:launch-program command)
    (echo "Brightness set to ~A%" clamped-percentage)))

#+linux
(defun get-current-volume ()
  "Get current volume as percentage"
  (parse-integer
   (uiop:run-program
    "pamixer --get-volume"
    :output :string)
   :junk-allowed t))

#+linux
(defun set-volume-percentage (percentage)
  "Set volume to given percentage"
  (let* ((clamped-percentage (max 0 (min 100 percentage)))
         (command
           (format nil "pamixer --set-volume ~A" clamped-percentage)))
    (uiop:launch-program command)
    (echo "Volume set to ~A%" clamped-percentage)))

#+linux
(defun is-muted ()
  "Check if audio is currently muted"
  (string=
   (string-trim '(#\newline #\space)
                (uiop:run-program
                 "pamixer --get-mute"
                 :output :string))
   "true"))

;; brightness

#+linux
(define-command-global set-brightness ()
  "Prompt user to set brightness percentage"
  (let* ((current-percentage (get-current-brightness))
         (new-brightness
           (first
            (prompt
             :prompt (format nil "Current Brightness: ~A%. Enter new brightness (0-100): "
                            current-percentage)
             :sources (make-instance
                       'prompter:raw-source)))))
    (set-brightness-percentage (parse-integer new-brightness))))

#+linux
(define-command-global brightness-down ()
  "Decrease brightness by 5%"
  (let ((current-percentage (get-current-brightness)))
    (set-brightness-percentage (- current-percentage 5))))

#+linux
(define-command-global brightness-up ()
  "Increase brightness by 5%"
  (let ((current-percentage (get-current-brightness)))
    (set-brightness-percentage (+ current-percentage 5))))

;; volume

#+linux
(defvar *previous-volume* 100
  "Stores the volume level before muting")

#+linux
(define-command-global set-volume ()
  "Prompt user to set volume percentage"
  (let* ((current-volume (get-current-volume))
         (new-volume
           (first
            (prompt
             :prompt (format nil "Current Volume: ~A%. Enter new volume (0-100): "
                            current-volume)
             :sources (make-instance
                       'prompter:raw-source)))))
    (set-volume-percentage (parse-integer new-volume))))

#+linux
(define-command-global volume-down ()
  "Decrease volume by 5%"
  (let ((current-volume (get-current-volume)))
    (set-volume-percentage (- current-volume 5))))

#+linux
(define-command-global volume-up ()
  "Increase volume by 5%"
  (let ((current-volume (get-current-volume)))
    (set-volume-percentage (+ current-volume 5))))

#+linux
(define-command-global toggle-mute ()
  "Toggle mute. Remembers previous volume when muting."
  (if (is-muted)
      (progn
        (set-volume-percentage *previous-volume*)
        (uiop:launch-program "pamixer --unmute")
        (echo "Unmuted. Volume restored to ~A%" *previous-volume*))
      (progn
        (setf *previous-volume* (get-current-volume))
        (uiop:launch-program "pamixer --mute")
        (echo "Muted"))))

;; backlight

#+linux
(define-command-global keyboard-backlight-down ()
  "Decrease keyboard backlight"
  (uiop:launch-program "brightnessctl -d '*::kbd_backlight' s 5%-")
  (echo "Keyboard backlight decreased"))

#+linux
(define-command-global keyboard-backlight-up ()
  "Increase keyboard backlight"
  (uiop:launch-program "brightnessctl -d '*::kbd_backlight' s +5%")
  (echo "Keyboard backlight increased"))
