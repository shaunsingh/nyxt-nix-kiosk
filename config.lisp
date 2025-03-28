(in-package #:nyxt-user)

;;; this file was created and edited in NYXT with ace-mode

;;; MODES

(defvar *buffer-modes*
  '(vi-normal-mode)
 "Modes to enable in buffer by default")

;; don't hint images
(define-configuration nyxt/mode/hint:hint-mode
  ((nyxt/mode/hint:hints-alphabet "DSJKHLFAGNMXCWEIO")
   (nyxt/mode/hint:hints-selector "a, button, input, textarea, details, select")))

;; add custom user agent and block utm
(define-configuration nyxt/mode/reduce-tracking:reduce-tracking-mode
  ((nyxt/mode/reduce-tracking:query-tracking-parameters
    (append '("utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content")
            %slot-value%))
    (preferred-user-agent
     "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36")))

;;; COLORSCHEME

(defmacro define-palette (&rest colors)
  "Helper macro to set global variables for `theme' colors"
  `(progn ,@(loop for (name hex)
                  in colors
                  collect `(defparameter ,name ,hex "Color used for `theme'"))))

(defun make-important (property)
  (str:concat property " !important"))

;; oxocarbon dark
(define-palette (*base00-* "#161616")
                (*base01-* "#262626")
                (*base02-* "#393939")
                (*base03-* "#525252")
                (*base04-* "#dde1e6")
                (*base05-* "#f2f4f8")
                (*base06-* "#ffffff")
                (*base07-* "#08bdba")
                (*base08-* "#3ddbd9")
                (*base09-* "#78a9ff")
                (*base0A-* "#ee5396")
                (*base0B-* "#33b1ff")
                (*base0C-* "#ff7eb6")
                (*base0D-* "#42be65")
                (*base0E-* "#be95ff")
                (*base0F-* "#82cfff")
                (*font* "SF Pro Display")
                (*mono* "Liga SFMono Nerd Font"))

;; internal pages
(define-configuration :web-buffer
  ((style
    (theme:themed-css (theme *browser*) 
      `(body
        :background-color ,*base00-*
        :color ,*base04-*
        :font-family ,*font*
        :margin "4% 6%")
      `("details > summary"
        :list-style "none")
      `("h1"
        :color ,*base06-*)
      `("#subtitle"
        :color ,*base0C-*)
      `("h1, #subtitle"
        :font-size "63px"
        :margin-bottom "-9px")
      `("h2"
        :font-size "27px"
        :margin "0"
        :margin-bottom "-9px"
        :color ,*base0C-*)
      `("h3"
        :font-size "18px"
        :color ,*base0E-*)
      `("h4"
        :color ,*base0F-*)
      `("h5"
        :color ,*base09-*)
      `("pre"
        :padding "9px"
        :padding-top "6px"
        :padding-bottom "12px")
      `("h2, h3, h4, h5, p"
        :margin-left "9px") 
      `("a, li, ul, pre"
        :color ,*base05-*)
      `(".action"
        :color ,*base0B-*)
      `("a code, p code, code, pre"
        :text-wrap "wrap"
        :font-family ,*mono*
        :background-color ,*base01-*
        :color ,*base06-*)
      `("a:hover, a:active"
        :color ,*base06-*)
      `("#buttons"
        :margin-left "13px")
      `(".button"
        :margin "3px")
      `("hr, .button"
        :background-color ,*base01-*
        :border-color ,*base01-*
        :color ,*base04-*
        :border-radius "0")
      `(".button:hover"
        :background-color ,*base02-*
        :color ,*base06-*)))))

;; modeline
(define-configuration :status-buffer
  ((height 36)
   (style
    (theme:themed-css (theme *browser*)
      `(*
        :font-family ,*mono*
        :font-size "11px")
      `(body
        :margin "9px"
        :margin-top "11px"
        :background-color ,*base02-* 
        :color ,*base05-*)
      `("#container"
        :display "flex"
        :white-space "nowrap"
        :overflow "hidden")
      `("#vi-mode, #buffers, #load, #percentage, #url, #tab, #modes"
        :padding-left "9px")
      `("#modes"
        :color "#a2a9b0")
      `(button
        :all "unset")
      `((:and (:or .button .tab "#url") :hover)
        :font-weight "bold"
        :cursor "pointer")))))

;; prompt
(define-configuration :prompt-buffer
  ((style
    (theme:themed-css (theme *browser*)
      `(*
        :font-family ,*mono*
        :font-size "13px")
      `(body
        :background-color ,*base00-*
        :color ,*base04-*
        :margin "0")
      '("#root"
        :height "100%"
        :display "grid"
        :grid-template-rows "auto 1fr")
      `("#prompt-area"
        :background-color ,*base02-*
        :color ,*base05-*
        :overflow "hidden"
        :white-space "nowrap"
        :display "grid"
        :grid-template-columns "auto auto 1fr auto auto")
      `("#prompt"
        :text-overflow "ellipsis")
      `("#prompt, #prompt-input, #prompt-modes, #close-button"
        :padding "3px"
        :padding-top "6px"
        :padding-left "9px")
      `("#prompt-modes, #close-button"
        :padding-right "3px"
        :padding-left "3px"
        :background-color ,*base01-*)
      `("#prompt-extra"
        :padding-right "9px")
      `("#prompt-modes"
        :padding-left "9px")
      `("#prompt-input"
        :background-color ,*base01-*
        :min-width "10ch")
      `("#close-button"
        :text-align "right")
      `(button
        :color ,*base04-*
        :background "transparent"
        :text-decoration "none"
        :border "none"
        :font "inherit"
        :outline "inherit")
      `((:and .button :hover)
        :cursor "pointer"
        :color ,*base06-*
        :font-weight "bold")
      `(".button svg path"
        :stroke ,*base04-*)
      `(".button:hover svg path"
        :stroke ,*base06-*)
      `((:and .button (:or :visited :active))
        :color ,*base04-*)
      `(input
        :background-color ,*base01-*
        :padding "0"
        :border-image-width "0")
      `("#input"
        :border "none"
        :color ,*base06-*
        :outline "none"
        :width "100%"
        :autofocus "true")
      '(".source"
        :margin-top "2px")
      `(".source-name"
        :background-color ,*base01-*
        :color ,*base05-*
        :display "flex"
        :justify-content "space-between"
        :align-items "stretch")
      '(".source-name > div"
        :line-height "26px")
      '(".source-name > div > button"
        :padding "5px 5px 5px 0px"
        :min-height "100%")
      `("#next-source > svg"
        :margin-left "9px")
      `("#next-source > svg, #previous-source > svg"
        :stroke ,*base06-*
        :margin-bottom "2px"
        :height "5px")
      '("#previous-source"
        :padding 0)
      '("#next-source"
        :padding 0)
      `("#suggestions"
        :color ,*base05-*
        :margin-right "3px"
        :overflow "hidden")
      `(".suggestion-and-mark-count"
        :font-family ,*mono*)
      `(".source-content"
        :box-sizing "border-box"
        :padding-left "16px"
        :margin-left "2px"
        :width "100%"
        :table-layout "fixed"
        (td
         :color ,*base05-*
         :overflow "hidden"
         :text-overflow "ellipsis"
         :white-space "nowrap"
         :height "20px"
         :padding-left "4px")
        ("tr:not(:first-child)"
         :font-family ,*mono*)
        ("tr:hover"
         :background-color ,*base0C-*
         :color ,*base00-*
         :cursor "pointer"
         :font-weight "bold")
        (th
         :background-color ,*base01-*
         :color ,*base06-*
         :font-weight "normal"
         :padding-left "4px"
         :text-align "left"))
      `("#selection"
        :background-color ,*base0B-*
        :color ,(make-important *base00-*))
      `(.marked
        :background-color ,*base0B-*
        :color ,(make-important *base00-*))
      `(.selected
        :background-color ,*base06-*
        :color ,(make-important *base00-*))))))

;; message buffer
(define-configuration (window)
 ((message-buffer-height 21)
  (message-buffer-style
   (theme:themed-css (theme *browser*)
    `(*
      :font-family ,*mono*
      :font-size "11px")
    `(body
      :background-color ,*base00-*
      :color ,*base05-*
      :padding 0
      :padding-left "9px"
      :margin "3px")))))

;; gopher etc. 
(define-configuration nyxt/mode/small-web:small-web-mode
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              `("pre"
                :background-color ,*base00-*)
              `("a.button.search"
                :color ,*base04-*
                :border-color ,*base04-*)
              `("a.button.error"
                :color ,*base0C-*
                :border-color ,*base0C-*))))))

;;; STARTPAGE

;; largely taken from fruit-of-the-day package, credit
(defparameter list-of-fruits
  (list "abiu"
        "açaí"
        "acerola"
        "ackee"
        "african cucumber"
        "apple"
        "apricot"
        "avocado"
        "banana"
        "bilberry"
        "blackberry"
        "blackcurrant"
        "black sapote"
        "blueberry"
        "boysenberry"
        "breadfruit"
        "buddha's hand (fingered citron)"
        "cactus pear"
        "canistel"
        "cempedak"
        "cherimoya (Custard Apple)"
        "cherry"
        "chico fruit"
        "cloudberry"
        "coco De Mer"
        "coconut"
        "crab apple"
        "cranberry"
        "currant"
        "damson"
        "date"
        "dragonfruit (or Pitaya)"
        "durian"
        "egg Fruit"
        "elderberry"
        "feijoa"
        "fig"
        "finger Lime (or Caviar Lime)"
        "goji berry"
        "gooseberry"
        "grape"
        "raisin"
        "grapefruit"
        "grewia asiatica (phalsa or falsa)"
        "guava"
        "hala Fruit"
        "honeyberry"
        "huckleberry"
        "jabuticaba"
        "jackfruit"
        "jambul"
        "japanese plum"
        "jostaberry"
        "jujube"
        "juniper berry"
        "kaffir Lime"
        "kiwano (horned melon)"
        "kiwifruit"
        "kumquat"
        "lemon"
        "lime"
        "loganberry"
        "longan"
        "loquat"
        "lulo"
        "lychee"
        "magellan Barberry"
        "mamey Apple"
        "mamey Sapote"
        "mango"
        "mangosteen"
        "marionberry"
        "melon"
        "cantaloupe"
        "galia melon"
        "honeydew"
        "mouse melon"
        "musk melon"
        "watermelon"
        "miracle fruit"
        "monstera deliciosa"
        "mulberry"
        "nance"
        "nectarine"
        "orange"
        "blood orange"
        "clementine"
        "mandarine"
        "tangerine"
        "papaya"
        "passionfruit"
        "pawpaw"
        "peach"
        "pear"
        "persimmon"
        "plantain"
        "plum"
        "prune (dried plum)"
        "pineapple"
        "pineberry"
        "plumcot (or Pluot)"
        "pomegranate"
        "pomelo"
        "purple mangosteen"
        "quince"
        "raspberry"
        "salmonberry"
        "rambutan (or Mamin Chino)"
        "redcurrant"
        "rose apple"
        "salal berry"
        "salak"
        "satsuma"
        "shine Muscat or Vitis Vinifera"
        "sloe or Hawthorn Berry"
        "soursop"
        "star apple"
        "star fruit"
        "strawberry"
        "surinam cherry"
        "tamarillo"
        "tamarind"
        "tangelo"
        "tayberry"
        "ugli fruit"
        "white currant"
        "white sapote"
        "yuzu"
        "bell pepper"
        "chile pepper"
        "corn kernel"
        "cucumber"
        "eggplant"
        "jalapeño"
        "olive"
        "pea"
        "pumpkin"
        "squash"
        "tomato"
        "zucchini"))

;; nice words
(defparameter list-of-pretty-words
  (list "lovely"
        "wonderful"
        "delightful"
        "beautiful"
        "pleasant"
        "adorable"
        "sweet"
        "delicious"
        "charming"
        "fantastic"
        "gorgeous"
        "heavenly"
        "magnificent"
        "radiant"
        "splendid"
        "exquisite"
        "enchanting"
        "serene"
        "blissful"
        "harmonious"
        "majestic"
        "tranquil"
        "whimsical"
        "ethereal"
        "celestial"
        "idyllic"
        "mesmerizing"
        "spellbinding"
        "captivating"
        "fascinating"
        "riveting"
        "enthralling"
        "mesmerizing"
        "inspiring"))

(defun fruit-of-the-day-message ()
  (flet ((capitalize-word (word)
           (concatenate 'string (string-upcase (subseq word 0 1))
                              (subseq word 1))))
    (let* ((current-time (local-time:now))
           (current-day (aref local-time:+day-names+
                              (local-time:timestamp-day-of-week current-time)))
           (current-fruit (nth (mod (local-time:day-of current-time)
                                    (length list-of-fruits))
                               list-of-fruits))
           (matching-words (remove-if-not (lambda (word)
                                            (char= (char word 0)
                                                   (char current-fruit 0)))
                                          list-of-pretty-words))
           (word (if matching-words
                     (nth (random (length matching-words)) matching-words)
                     (nth (random (length list-of-pretty-words))
                          list-of-pretty-words))))
      (format nil "Have ~A ~A ~A ~A!"
              (if (member (char (string word) 0) '(#\a #\e #\i #\o #\u))
                  "an" "a")
              (capitalize-word word)
              (capitalize-word current-fruit)
              current-day))))

;; modified from time.lisp
(defun sort-by-time (sequence &key (key #'last-access))
  "Return a timely ordered SEQUENCE by KEY.  More recent elements come first."
  (sort sequence #'local-time:timestamp> :key key))

(define-internal-page-command-global startpage ()
    (buffer "*startpage*")
  "my custom startpage"
  (flet ((list-bookmarks (&key (limit 6) (separator " → "))
           (spinneret:with-html-string
             (let ((mode (make-instance 'nyxt/mode/bookmark:bookmark-mode)))
               (alexandria:if-let ((bookmark-content (ignore-errors (files:content (nyxt/mode/bookmark:bookmarks-file mode)))))
                 (dolist (bookmark (serapeum:take limit (the list (sort-by-time bookmark-content :key #'nyxt/mode/bookmark:date))))
                   (:li (title bookmark) separator
                        (:a :href (render-url (url bookmark))
                            (render-url (url bookmark)))))
                 (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/mode/bookmark:bookmarks-file mode)))))))))
         ;; reimplemented from nyxt 2.x
;;          (history-html-list (&key (limit 6) (separator " → "))
;;            (spinneret:with-html-string
;;              (let ((history-entries (subseq (history-vector *browser*) 
;;                                            0 
;;                                            (min limit (length (history-vector *browser*))))))
;;                (if (plusp (length history-entries))
;;                    (dolist (entry history-entries)
;;                      (:li (title entry) separator
;;                           (:a :href (render-url (url entry))
;;                               (render-url (url entry)))))
;;                    (:p "No history entries."))))))
    (let ((current-year (local-time:timestamp-year (local-time:now)))
          (dashboard-style (theme:themed-css (theme *browser*)
                              `("#motto"
                                :font-size "27px"
                                :margin "18px"
                                :color ,*base08-*)
                              `("#buttons"
                                :margin-top "18px"
                                :font-size "18px")
                              `("#copyright"
                                :font-family ,*mono*
                                :position "absolute"
                                :text-align "right"
                                :bottom "1.5em"
                                :right "1.5em"))))
     (spinneret:with-html-string
       (:nstyle dashboard-style)
       (:div :id "container"
        (:h1 "Welcome to " (:span :id "subtitle" "NYXT"))
         (:div :id "buttons"
          (:nbutton :text "Repl"
           '(nyxt/mode/repl:repl))
          (:nbutton :text "Manual"
           '(make-buffer-focus :url (nyxt-url 'manual)))
          (:nbutton :text "Changelog"
           '(make-buffer-focus :url (nyxt-url 'changelog)))
          (:nbutton :text "Bookmarks"
           '(nyxt/mode/bookmark:list-bookmarks))
          (:nbutton :text "Annotations"
           '(nyxt/mode/annotate:show-annotations)))
        (:div :id "motto"
         "私たちのミッションは"
         (:br)
         "先端工学を用いて上質で"
         (:br)
         "機能的なデザインの"
         (:br)
         "製品を作り出すことです。")
        (:h2 "Bookmarks")
        (:ul (:raw (list-bookmarks :limit 9)))
        ;;(:h3 "History")
        ;;(:ul (:raw (history-html-list :limit 9)))
        (:h3 (fruit-of-the-day-message))
        (:div :id "copyright"
          (format nil "version ~a ~a" (name nyxt::*renderer*) nyxt::+version+)
          (:br)
          (format nil "lisp ~a ~a" (lisp-implementation-type) (lisp-implementation-version))
          (:br)
          (format nil "host ~a@~a" (software-type) (software-version))
          (:br)
          (format nil "Atlas Engineer LLC, 2018-~a" current-year)
          (:br)
          (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+)))))))

(define-configuration browser
  ((default-new-buffer-url (quri:uri "nyxt:nyxt-user:startpage"))))

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

;;; BRIGHTNESSCTL 

#+linux
(define-command-global set-brightness ()
  "Set brightness using brightnessctl"
  (let* ((current-brightness 
           (parse-integer 
            (uiop:run-program 
             "brightnessctl g" 
             :output :string) 
            :junk-allowed t))
         (max-brightness 
           (parse-integer 
            (uiop:run-program 
             "brightnessctl m" 
             :output :string) 
            :junk-allowed t))
         (current-percentage 
           (floor (* 100 (/ current-brightness max-brightness))))
         (new-brightness 
           (first 
            (prompt 
             :prompt (format nil "Current Brightness: ~A%. Enter new brightness (0-100): " 
                              current-percentage)
             :sources (make-instance 
                       'prompter:raw-source)))))
    (let* ((brightness-value 
             (floor (* max-brightness (/ (parse-integer new-brightness) 100.0))))
           (command 
             (format nil "brightnessctl s ~A%" brightness-value)))
      (uiop:launch-program command)
      (echo "Brightness set to ~A%" new-brightness))))

;;; PAMIXER

#+linux
(define-command-global set-volume ()
  "Prompt user to set volume percentage"
  (let* ((current-volume 
           (parse-integer 
            (uiop:run-program 
             "pamixer --get-volume" 
             :output :string) 
            :junk-allowed t))
         (new-volume 
           (first 
            (prompt 
             :prompt (format nil "Current Volume: ~A%. Enter new volume (0-100): " 
                              current-volume)
             :sources (make-instance 
                       'prompter:raw-source)))))
    (let ((command 
            (format nil "pamixer --set-volume ~A" 
                    (parse-integer new-volume))))
      (uiop:launch-program command)
      (echo "Volume set to ~A%" new-volume))))

;;; FETCH

(defun mem-total ()
    (float 
       (/ (parse-integer (string-trim "MemTotal:       kB " (uiop:read-file-line "/proc/meminfo" :at 0))) (* 1024 1024))))
(defun mem-free ()
  (float
    (/ (parse-integer (string-trim "MemFree:         kB" (uiop:read-file-line "/proc/meminfo" :at 1))) (* 1024 1024))))
(defun mem-cached ()
  (float
    (/ (parse-integer (string-trim "Cached:         kB" (uiop:read-file-line "/proc/meminfo" :at 4))) (* 1024 1024))))
(defun mem-used ()
   (- (mem-total) (+ (mem-cached) (mem-free))))

#+linux
(define-internal-page-command-global fetch ()
    (buffer "*fetch*")
  "my custom fetch"
  (let ((dashboard-style (theme:themed-css (theme *browser*)
                            `("#fetch"
                              :font-family ,*mono*
                              :font-size "18px"
                              :margin "18px"
                              :color ,*base05-*
                              :background-color ,*base01-*))))
   (spinneret:with-html-string
     (:nstyle dashboard-style)
     (:div :id "container"
      (:h1 "System " (:span :id "subtitle" "FETCH"))
      (:pre :id "fetch"
        (format nil "NYXT ~a ~a" (name nyxt::*renderer*) nyxt::+version+)
        (:br)
        (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version))
        (:br)
        (format nil "HOST: ~a@~a" (machine-instance) (software-version))
        (:br)
        (format nil "WM: ~a" (uiop:getenv "XDG_CURRENT_DESKTOP"))
        (:br)
        (format nil "THEME: ~a (~a) w/ ~a" (uiop:getenv "GTK_THEME") "oxocarbon" (uiop:getenv "XCURSOR_THEME"))
        (:br)
        (format nil "SHELL: ~a" (uiop:getenv "SHELL"))
        (:br)
        (format nil "RAM: ~,2f/~f GB" (mem-used) (fceiling (mem-total)))
        (:br)
        ;; doesn't work on m1
        ;; (format nil "CPU: ~a" (machine-version))
        "CPU: (8) @ 2.064GHz"
        (:br)
        (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))))))

;;; ACE

;; editor-mode was removed, reimplement it. modified from editor.lisp 
(define-mode my-editor-mode ()
  "General-purpose editor mode, meant to be subclassed")

(defgeneric get-content (editor-submode)
  (:method ((editor my-editor-mode))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode."))
  (:documentation "Get the content of the EDITOR-SUBMODE as a string."))

(defgeneric set-content (editor-submode content)
  (:method ((editor my-editor-mode) (content t))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode.
See `describe-class my-editor-mode' for details."))
  (:documentation "Set the content of EDITOR-SUBMODE to the string CONTENT."))

(defgeneric markup (editor-submode)
  (:method ((editor my-editor-mode))
    (spinneret:with-html-string
      (:head
       (:nstyle (style (buffer editor))))
      (:body
       (:p "Please configure an editor mode to use an editor buffer. See "
           (:code "describe-class") " for " (:code "my-editor-buffer")
           " to see the list of functions to implement."))))
  (:documentation "Return an HTML string representation of the file to be edited."))

(define-class my-editor-buffer (network-buffer ;; questionable, but needed for `buffer-load'.
                             context-buffer modable-buffer document-buffer input-buffer)
  ((nyxt:title "*Editor*"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Buffer to edit files"))

(defmethod nyxt:default-modes :around ((buffer my-editor-buffer))
  (set-difference (call-next-method) '(document-mode base-mode)))

(defmethod file ((buffer my-editor-buffer))
  (uiop:parse-native-namestring (quri:uri-path (url buffer))))

(define-internal-scheme "editor"
    (lambda (url buffer)
      (let ((mode (find-submode 'my-editor-mode buffer))
            (file (quri:uri-path (quri:uri url))))
        (uiop:chdir (uiop:pathname-directory-pathname file))
        (run-thread "editor content setting"
          (sleep 2)
          (set-content mode (uiop:read-file-string file)))
        (markup mode))))

(defmethod editor ((my-editor-buffer my-editor-buffer))
  (let ((mode (find-submode 'my-editor-mode my-editor-buffer)))
    (unless (eq 'my-editor-mode (serapeum:class-name-of mode))
      mode)))

(defmethod write-file-with-editor ((buffer my-editor-buffer) &key (if-exists :error))
  (cond
    ((editor buffer)
     (handler-case
         (alexandria:write-string-into-file (get-content (editor buffer))
                                            (file buffer)
                                            :if-exists if-exists)
       (file-error (e)
         (echo-warning "Cannot write ~a: ~a" (file buffer) e)
         nil)))
    (t
     (echo-warning "Editor buffer cannot write file without configured editor mode.")
     nil)))

(defun prompt-for-file ()
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

(define-command my-editor-open-file (&key (buffer (current-buffer)) (file (prompt-for-file)))
  "Open my file.

BUFFER is of type `my-editor-buffer'."
  (buffer-load (quri:make-uri :scheme "editor" :path file) :buffer buffer))

(define-command my-editor-write-file (&key (buffer (current-buffer)))
  "Write my file to storage.

BUFFER is of type `my-editor-buffer'."
  (if (uiop:file-exists-p (file buffer))
      (if-confirm ((format nil "Overwrite ~s?" (file buffer))
                   :yes "overwrite" :no "cancel")
          (echo "File ~s ~:[not ~;~]saved."
                (file buffer) (write-file-with-editor buffer :if-exists :overwrite)))
      (echo "File ~s ~:[not ~;~]saved." (file buffer) (write-file-with-editor buffer))))

(define-command-global edit-file (&optional (file (prompt-for-file)))
  "Open a new editor and query my FILE to edit in it."
  (set-current-buffer (make-instance 'my-editor-buffer
                                     :url (quri:make-uri :scheme "editor" :path file))))

(defun prompt-for-editor-user-file ()
  (uiop:native-namestring
   (files:expand
    (prompt1 :prompt "Edit user file"
             :sources 'nyxt::user-file-source))))

(define-command-global edit-user-file (&optional (file (prompt-for-editor-user-file)))
  (set-current-buffer (make-instance 'my-editor-buffer
                                     :url (quri:make-uri :scheme "editor" :path file))))

(define-auto-rule '(match-scheme "editor")
  :included '(my-editor-mode))

;; extend our new mode. heavily inspired by nx-ace
(define-mode ace-mode (my-editor-mode) ;; nyxt/mode/passthrough:passthrough-mode) ; TODO buggy 
  "Mode for usage with the Ace editor."
  ((style
    (theme:themed-css (theme *browser*)
      ("#editor"
       :position "absolute"
       :top "0"
       :right "0"
       :bottom "0"
       :left "0")))
   (extensions
    nil
    :type list)
   (keybindings
    nil
    :type (maybe string))
   (epilogue
    nil
    :type (maybe string))))

(defmethod markup ((ace ace-mode))
  (spinneret:with-html-string
    (:head
     (:style (style ace)))
    (:body
     (:script
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.39.1/ace.min.js"
      :crossorigin "anonymous"
      :type "text/javascript"
      :charset "utf-8"
      "")
     (dolist (ext (extensions ace))
       (:script
        :src (quri:render-uri (quri:uri ext))
        :crossorigin "anonymous"
        :type "text/javascript"
        :charset "utf-8"
        ""))
     (:div :id "editor" "")
     (:script
      (:raw
       (ps:ps
         (defparameter editor (ps:chain ace (edit "editor")))
         (ps:chain editor (set-keyboard-handler
                           (ps:@ (require (ps:lisp (keybindings ace))) handler))))))
     (:script
      (:raw (epilogue ace))))))

(defmethod set-content ((ace ace-mode) content)
  (ps-eval :buffer (buffer ace)
    (ps:chain editor session (set-value (ps:lisp content)))))

(defmethod get-content ((ace ace-mode))
  (ps-eval :buffer (buffer ace) (ps:chain editor (get-value))))

(defmethod set-option ((ace ace-mode) option value)
  (ps-eval :buffer (buffer ace)
    (ps:chain editor (set-option (ps:lisp option) (ps:lisp value)))))

(defun options ()
  (alexandria:hash-table-keys (ps-eval (ps:chain editor (get-options)))))

(define-configuration ace-mode
  ((keyscheme-map
    (define-keyscheme-map "my-editor-mode" ()
      nyxt/keyscheme:default
      (list
       "C-r" 'reload-current-buffer
       "f11" 'toggle-fullscreen)
      nyxt/keyscheme:cua
      (list
       "C-o" 'my-editor-open-file
       "C-s" 'my-editor-write-file
       "C-w" 'delete-current-buffer
       "C-tab" 'switch-buffer)
      nyxt/keyscheme:emacs
      (list
       "C-x C-f" 'my-editor-open-file
       "C-x C-s" 'my-editor-write-file
       "C-x C-k" 'delete-current-buffer
       "C-x b" 'switch-buffer)
      nyxt/keyscheme:vi-normal
      (list
       "C-o" 'my-editor-open-file
       "C-s" 'my-editor-write-file
       "C-w" 'delete-current-buffer
       "C-tab" 'switch-buffer
       "C-space" 'execute-command))) ;; due to passthrough not working
   (:toggler-command-p nil)
   (style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
              `(".oxocarbon"
               :color ,*base05-*
               :background-color ,*base00-*)
              `(".oxocarbon .ace_gutter"
               :color ,*base03-*
               :background-color "#131313")
              `(".oxocarbon .ace_print-margin"
               :width "1px"
               :background-color ,*base01-*)
              `(".oxocarbon .ace_cursor"
               :color ,*base06-*)
              `(".oxocarbon .ace_marker-layer .ace_selection"
               :background-color ,*base02-*)
              `(".oxocarbon .ace_marker-layer .ace_step"
               :background-color ,*base0A-*)
              `(".oxocarbon .ace_marker-layer .ace_active-line"
               :background-color ,*base01-*)
              `(".oxocarbon .ace_gutter-active-line"
               :color ,*base06-*)
              `(".oxocarbon .ace_marker-layer .ace_selected-word"
               :background-color ,*base02-*)
              `(".oxocarbon .ace_fold"
               :color ,*base04-*)
              ;; worker extension
              `(".oxocarbon.ace_editor.ace_autocomplete"
               :border "0"
               :background-color ,*base01-*
               :color ,*base04-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_marker-layer .ace_active-line"
               :border "0"
               :background-color ,*base02-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_completion-meta"
               :border "0"
               :color ,*base04-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_line-hover"
               :border "0"
               :background-color ,*base0C-*
               :color ,*base05-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_completion-highlight"
               :color ,*base06-*)
              ;; token styles
              `(".oxocarbon .ace_comment"
               :color ,*base03-*)
              `(".oxocarbon .ace_keyword"
               :color ,*base0C-*)
              `(".oxocarbon .ace_constant.ace_numeric"
               :color ,*base0F-*)
              `(".oxocarbon .ace_constant.ace_character"
               :color ,*base07-*)
              `(".oxocarbon .ace_constant.ace_character.ace_escape"
               :color ,*base08-*)
              `(".oxocarbon .ace_constant.ace_character.ace_language"
               :color ,*base09-*)
              `(".oxocarbon .ace_constant.ace_character.ace_other"
               :color ,*base09-*)
              `(".oxocarbon .ace_support.ace_function"
               :font-weight "bold"
               :color ,*base0C-*)
              `(".oxocarbon .ace_support.ace_constant"
               :color ,*base07-*)
              `(".oxocarbon .ace_support.ace_class"
               :color ,*base08-*)
              `(".oxocarbon .ace_support.ace_type"
               :color ,*base08-*)
              `(".oxocarbon .ace_storage"
               :color ,*base09-*)
              `(".oxocarbon .ace_storage.ace_type"
               :color ,*base08-*)
              `(".oxocarbon .ace_invalid"
               :color ,*base0A-*)
              `(".oxocarbon .ace_invalid.ace_deprecated"
               :color ,*base03-*)
              `(".oxocarbon .ace_string"
               :color ,*base0E-*)
              `(".oxocarbon .ace_variable"
               :color ,*base0F-*)
              `(".oxocarbon .ace_variable.ace_parameter"
               :color ,*base04-*)
              `(".oxocarbon .ace_entity.ace_other.ace_attribute-name"
               :color ,*base0B-*)
              `(".oxocarbon .ace_entity.ace_name.ace_tag"
               :color ,*base0F-*)
              `(".oxocarbon .ace_invisible"
               :color ,*base03-*))))
   (:keybindings "ace/keyboard/vim")
   (extensions
     (mapcar
       (lambda (name)
         (quri:merge-uris (quri:uri name)
        (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.39.1/")))
       '(;; vim keybinding support
         "keybinding-vim.min.js"
         ;; language modes
         "mode-java.min.js"
         "mode-lisp.min.js"
         "mode-nix.min.js"
         "mode-c_cpp.min.js"
         "mode-rust.min.js"
         "mode-makefile.min.js"
         "mode-markdown.min.js"
         "mode-sh.min.js"
         "mode-lua.min.js"
         "mode-python.min.js"
         "mode-ocaml.min.js"
         ;; language snippets
         "snippets/java.min.js"
         "snippets/nix.min.js"
         "snippets/c_cpp.min.js"
         "snippets/rust.min.js"
         "snippets/makefile.min.js"
         "snippets/markdown.min.js"
         "snippets/sh.min.js"
         "snippets/lua.min.js"
         "snippets/python.min.js"
         ;; language workers
         "worker-base.min.js"
         ;; extensions
         "ext-language_tools.min.js"   ;; basic autocompletion/snippets
         "ext-whitespace.min.js"       ;; detect spacing/indent
         "ext-settings_menu.min.js"    ;; view and adjust settings
         "ext-keybinding_menu.min.js"  ;; view and adjust keybindings
         "ext-modelist.min.js")))      ;; detect mode based on filepath
    (epilogue
      (str:concat
       (ps:ps
         (flet ((req (ext)
                  (ps:chain ace (require ext)))
                (em-bind (key command)
                  (ps:chain editor commands (bind-key key command)))
                (vi-map (key command mode)
                  (ps:chain (req "ace/keyboard/vim") -code-mirror -vim (map key command mode)))
                (vi-noremap (key command mode)
                  (ps:chain (req "ace/keyboard/vim") -code-mirror -vim (noremap key command mode)))
                (vi-define-ex (name pre fn)
                  (ps:chain (req "ace/keyboard/vim") -code-mirror -vim (define-ex name pre fn))))
           ;; set theme
           (ps:chain editor (set-theme (parenscript:create css-class "oxocarbon" is-dark t)))
           ;; load extensions
           (req "ace/ext/language_tools")
           (req "ace/ext/whitespace")
           (ps:chain (req "ace/ext/settings_menu") (init editor))
           (ps:chain (req "ace/ext/keybinding_menu") (init editor))
           (ps:chain editor session
                     (set-mode (ps:chain (req "ace/ext/modelist")
                                         (get-mode-for-path (ps:@ window location href)) mode)))
           ;; editor config 
           (ps:chain editor (set-option "cursorStyle" "wide"))          ;; static cursor
           (ps:chain editor (set-option "readOnly" nil))                ;; set read and write file
           (ps:chain editor (set-option "fontSize" 15))                 ;; bigger default font
           (ps:chain editor (set-option "showLineNumbers" nil))         ;; disable line numbers
           (ps:chain editor (set-option "showPrintMargin" t))           ;; enable print margin (colorline)
           (ps:chain editor (set-option "wrap" "free"))                 ;; wrap to end of screen 
           (ps:chain editor (set-option "displayIndentGuides" nil))     ;; disable indent markers
           (ps:chain editor (set-option "hScrollBarAlwaysVisible" nil)) ;; don't always show scrollbar (h)
           (ps:chain editor (set-option "vScrollBarAlwaysVisible" nil)) ;; don't always show scrollbar (v)
           (ps:chain editor (set-option "useSoftTabs" t))               ;; use spaces instead of tabs
           (ps:chain editor (set-option "enableSnippets" t))            ;; enable snippet support
           (ps:chain editor (set-option "enableBasicAutocompletion" t)) ;; enable autocomplete support (basic)
           (ps:chain editor (set-option "enableLiveAutocompletion" t))  ;; enable autocomplete support (live)
           (ps:chain editor (set-option "highlightActiveLine" t))       ;; highlight current line
           ;; vim bindings
           (vi-noremap "j" "gj" "normal")
           (vi-noremap "k" "gk" "normal")
           ;; vim ex commands (ace)
           (vi-define-ex "help" "h" (lambda ()
                                        (ps:chain editor (show-keyboard-shortcuts))))
           (vi-define-ex "settings" "se" (lambda ()
                                            (ps:chain editor (show-settings-menu))))
           ;; vim ex commands (nyxt
           ;; (vi-define-ex "write" "w" (editor-write-file))
           ;; (vi-define-ex "quit" "q" (delete-current-buffer))
           ;; (vi-define-ex "wq" "wq" (lambda ()
           ;;                           (editor-write-file)
           ;;                           (delete-current-buffer)))
           ;; (vi-define-ex "edit" "e" (editor-open-file))
           ;; (vi-define-ex "buffer" "b" (switch-buffer))
           ;; load workers
           (req "ace/worker/base")))))))

(defmethod nyxt:default-modes append ((buffer my-editor-buffer))
  "Add `my-editor-mode' and `ace-mode' to `my-editor-buffer' by default."
  (list 'my-editor-mode 'ace-mode))

;;; REPL

;; this was removed in pre-release4, re-implement

(define-configuration nyxt/mode/repl:repl-mode
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              `(".input-area"
                :background-color ,*base02-*)
              `("#cells"
                :overflow "clip")
              `("code"
                :font-family ,*font*
                :font-size "18px"
                :background "transparent"
                :color ,*base05-*
                :margin "9px")
              `("textarea"
                :background-color ,*base01-*
                :color ,*base05-*
                :padding "9px"
                :padding-top "6px"
                :padding-bottom "12px")
              `(".cell-actions"
                :margin-left "13px")
              `("code, textarea, .cell-actions"
                :margin "9px 9px 0px 9px"))))))

;;; KAOMOJI

;;; MPV

(defun execute-mpv (link)
  (uiop:launch-program (list "mpv" link) :ignore-error-status t))

(define-command-global mpv-hint ()
  "Show a set of element hints, and go to the user inputted one in the
currently active buffer."
  (nyxt/mode/hint:query-hints
   "open video in mpv"
   (lambda (hint)
     (let ((hint (if (listp hint) (car hint) hint)))
       (echo "~A" hint)
       (case (type-of hint)
         (nyxt/dom:a-element
          (echo "MPV launched with ~a" (url hint))
          (execute-mpv (quri:render-uri (url hint))))
         (t
          (echo "failed to launch mpv")
          (print (type-of hint))
          (print hint)))))))

(define-command-global open-mpv ()
  "executes mpv on the current buffer"
  (execute-mpv (quri:render-uri (url (current-buffer)))))

(defmethod url-sources-no-suggestions ((buffer buffer) return-actions)
  (append
   (list (make-instance 'global-history-source :actions-on-return return-actions)
         (make-instance 'nyxt/mode/search-buffer:search-buffer-source :actions-on-return return-actions))
   (alexandria:mappend (alexandria:rcurry #'url-sources return-actions) (modes buffer))))

(define-command-global mpv-url (&key (prefill-current-url-p t))
  "open an url in mpv"
  (let ((history (set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (flet ((func (urls)
             (let* ((url (car urls))
                    (url-string
                      (cond ((typep url 'history-entry) (render-url (url url)))
                            ((stringp url)              url)
                            ((valid-url-p url)          (render-url url))
                            (t                          (render-url (url url))))))
               (echo "MPV launched with ~a" url)
               (execute-mpv url-string))))
      (prompt
       :prompt (format nil "Launch mpv on")
       :input (if prefill-current-url-p
                  (quri:render-uri (url (current-buffer))) "")
       :sources
       (url-sources-no-suggestions (current-buffer) (list #'func))
       :history history))))

;;; TOR

(define-mode tor-proxy-mode (nyxt/mode/proxy:proxy-mode)
  "Launch tor & set proxy to local Tor SOCKS5 proxy."
  ((uiop:launch-program "tor")
    (nyxt/mode/proxy:proxy (make-instance 
                            'proxy
                            :url (quri:uri "socks5://localhost:9050")
                            :allowlist '("localhost")
                            :proxied-downloads-p t))))

;;; ZOTERO

;;; SEARCH

;; all heavily inspired by nx-search-engine, credit

(defmacro define-search-engine (name (&key shortcut fallback-url base-search-url
                                        force-supply-p manual-delims-p completion-function
                                        documentation)
                                &body keywords)

  (flet ((supplied-p (symbol)
           (intern (format nil "~s-SUPPLIED-P" symbol)
                   (symbol-package symbol)))
         (make-cond (arg-name values)
           `(cond
              ,@(loop :for value :in values
                      :collect
                      `((equal ,arg-name ,(first value))
                        ,(second value))
                        :into clauses
                      :finally (return (append clauses (list `(t ,arg-name))))))))
    `(progn
       (defun ,name (&key
                       (fallback-url ,fallback-url)
                       (shortcut ,shortcut)
                       (completion-function ,completion-function)
                       (base-search-url ,base-search-url)
                       (manual-delims-p ,manual-delims-p)
                       (force-supply-p ,force-supply-p)
                       ,@(mapcar #'(lambda (k)
                                     (list (first k)                 ; name
                                           (if (eq (first (third k)) :function)
                                               nil
                                               (first (first (third k)))) ; default value
                                           (supplied-p (first k))))  ; supplied-p
                                 keywords))
         (declare (ignorable force-supply-p manual-delims-p
                             ,@(mapcar #'first keywords)
                             ,@(mapcar (alexandria:compose #'supplied-p #'first) keywords)))
         (make-instance
          'search-engine
          :shortcut shortcut
          :fallback-url fallback-url
          :completion-function completion-function
          :search-url (format nil "~a~{~a~}"
                              base-search-url
                              (delete
                               nil
                               (list
                                ,@(loop :for (arg-name uri-parameter values)
                                          :in keywords
                                        :collect
                                        `(when ,(or force-supply-p (supplied-p arg-name))
                                           (format nil (if manual-delims-p
                                                           "~a~a"
                                                           "&~a=~a")
                                                   ,uri-parameter
                                                   ,(if (eq (first values) :function)
                                                        `(funcall ,(second values) ,arg-name)
                                                        (make-cond arg-name values))))))))))
       ,@(when documentation
           `((setf (documentation (quote ,name) 'function) ,documentation))))))

(defmacro define-derived-search-engine (name (parent-engine &rest arguments) &optional documentation)
  `(progn
     (defun ,name (&rest args)
       ,documentation
       (apply (function ,parent-engine) (append args (list ,@arguments))))))

(defun make-google-completion (&key request-args)
  (make-search-completion-function
   :base-url "https://www.google.com/complete/search?q=~a&client=gws-wiz"
   :processing-function
   #'(lambda (results)
       (mapcar (alexandria:compose (alexandria:curry #'str:replace-using '("<b>" "" "</b>" ""))
                                   #'first)
               (first (json:decode-json-from-string
                       (str:replace-first "window.google.ac.h(" "" results)))))
   :request-args request-args))


(defvar *google-countries*
  '((:default "")
    (:afghanistan "AF")
    (:albania "AL")
    (:algeria "DZ")
    (:american-samoa "AS")
    (:andorra "AD")
    (:anguilla "AI")
    (:antartica "AQ")
    (:antigua-and-barbuda "AG")
    (:argentina "AR")
    (:armenia "AM")
    (:aruba "AW")
    (:australia "AU")
    (:austria "AT")
    (:azerbaijan "AZ")
    (:bahamas "BS")
    (:bahrain "BH")
    (:bangladesh "BD")
    (:barbados "BB")
    (:belarus "BY")
    (:belgium "BE")
    (:belize "BZ")
    (:benin "BJ")
    (:bermuda "BM")
    (:bhutan "BT")
    (:bolivia "BO")
    (:bosnia "BA")
    (:botswana "BW")
    (:bouvet-island "BV")
    (:brazil "BR")
    (:british-indian-ocean "IO")
    (:brunei "BN")
    (:bulgaria "BG")
    (:burkina-faso "BF")
    (:burundi "BI")
    (:cambodia "KH")
    (:cameroon "CM")
    (:canada "CA")
    (:cape-verde "CV")
    (:cayman-islands "KY")
    (:central-african-republic "CF")
    (:chad "TD")
    (:chile "CL")
    (:china "CN")
    (:christmas-island "CX")
    (:cocos-islands "CC")
    (:colombia "CO")
    (:comoros "KM")
    (:congo "CG")
    (:democratic-replublic-of-congo "CD")
    (:cook-islands "CK")
    (:costa-rica "CR")
    (:cote-divoire "CI")
    (:croatia "HR")
    (:cuba "CU")
    (:cyprus "CY")
    (:czech-republic "CZ")
    (:denmark "DK")
    (:djibouti "DJ")
    (:dominica "DM")
    (:dominican-republic "DO")
    (:east-timor "TP")
    (:ecuador "EC")
    (:egypt "EG")
    (:el-salvador "SV")
    (:equatorial-guinea "GQ")
    (:eritrea "ER")
    (:estonia "EE")
    (:ethiopia "ET")
    (:european-union "EU")
    (:falkland-islands "FK")
    (:faroe-islands "FO")
    (:fiji "FJ")
    (:finland "FI")
    (:france "FR")
    (:france-metropolitan "FX")
    (:french-guiana "GF")
    (:french-polynesia "PF")
    (:french-southern-territories "TF")
    (:gabon "GA")
    (:gambia "GM")
    (:georgia "GE")
    (:germany "DE")
    (:ghana "GH")
    (:gibraltar "GI")
    (:greece "GR")
    (:greenland "GL")
    (:grenada "GD")
    (:guadeloupe "GP")
    (:guam "GU")
    (:guatemala "GT")
    (:guinea "GN")
    (:guinea-bissau "GW")
    (:guyana "GY")
    (:haiti "HT")
    (:heard-island-mcdonald-islands "HM")
    (:vatican-city "VA")
    (:honduras "HN")
    (:hong-kong "HK")
    (:hungary "HU")
    (:iceland "IS")
    (:india "IN")
    (:indonesia "ID")
    (:iran "IR")
    (:iraq "IQ")
    (:ireland "IE")
    (:israel "IL")
    (:italy "IT")
    (:jamaica "JM")
    (:japan "JP")
    (:jordan "JO")
    (:kazakhstan "KZ")
    (:kenya "KE")
    (:kiribati "KI")
    (:democratic-peoples-republic-of-korea "KP")
    (:republic-of-korea "KR")
    (:kuwait "KW")
    (:kyrgyzstan "KG")
    (:lao "LA")
    (:latvia "LV")
    (:lebanon "LB")
    (:lesotho "LS")
    (:liberia "LR")
    (:libyan-arab-jamahiriya "LY")
    (:liechtenstein "LI")
    (:lithuania "LT")
    (:luxembourg "LU")
    (:macao "MO")
    (:macedonia "MK")
    (:madagascar "MG")
    (:malawi "MW")
    (:malaysia "MY")
    (:maldives "MV")
    (:mali "ML")
    (:malta "MT")
    (:marshall-islands "MH")
    (:martinique "MQ")
    (:mauritania "MR")
    (:mauritius "MU")
    (:mayotte "YT")
    (:mexico "MX")
    (:micronesia "FM")
    (:moldova "MD")
    (:monaco "MC")
    (:mongolia "MN")
    (:montserrat "MS")
    (:morocco "MA")
    (:mozambique "MZ")
    (:myanmar "MM")
    (:namibia "NA")
    (:nauru "NR")
    (:nepal "NP")
    (:netherlands "NL")
    (:netherlands-antilles "AN")
    (:new-caledonia "NC")
    (:new-zealand "NZ")
    (:nicaragua "NI")
    (:niger "NE")
    (:nigeria "NG")
    (:niue "NU")
    (:norkfolk-island "NF")
    (:northern-mariana-islands "MP")
    (:norway "NO")
    (:oman "OM")
    (:pakistan "PK")
    (:palau "PW")
    (:palestinian-territory "PS")
    (:panama "PA")
    (:papua-new-guinea "PG")
    (:paraguay "PY")
    (:peru "PE")
    (:philippines "PH")
    (:pitcairn "PN")
    (:poland "PL")
    (:portugal "PT")
    (:puerto-rico "PR")
    (:qatar "QA")
    (:reunion "RE")
    (:romania "RO")
    (:russian-federation "RU")
    (:rwanda "RW")
    (:saint-helena "SH")
    (:saint-kitts-and-nevis "KN")
    (:saint-lucia "LC")
    (:saint-pierre-and-miquelon "PM")
    (:saint-vincent-and-the-grenadines "VC")
    (:samoa "WS")
    (:san-marino "SM")
    (:sao-tome-and-principe "ST")
    (:saudi-arabia "SA")
    (:senagal "SN")
    (:serbia-and-montenegro "CS")
    (:seychelles "SC")
    (:sierra-leone "SL")
    (:singapore "SG")
    (:slovakia "SK")
    (:slovenia "SI")
    (:solomon-islands "SB")
    (:somalia "SO")
    (:south-africa "ZA")
    (:south-georgia "GS")
    (:spain "ES")
    (:sri-lanka "LK")
    (:sudan "SD")
    (:suriname "SR")
    (:svalbard-and-jan-mayen "SJ")
    (:swaziland "SZ")
    (:sweden "SE")
    (:switzerland "CH")
    (:syrian-arab-republic "SY")
    (:taiwan "TW")
    (:tajikistan "TJ")
    (:tanzania "TZ")
    (:thailand "TH")
    (:togo "TG")
    (:tokelau "TK")
    (:tonga "TO")
    (:trinidad-and-tobago "TT")
    (:tunisia "TN")
    (:turkey "TR")
    (:turkmenistan "TM")
    (:turks-and-caicos-islands "TC")
    (:tuvalu "TV")
    (:uganda "UG")
    (:ukraine "UA")
    (:united-arab-emirates "AE")
    (:united-kingdom "UK")
    (:united-states "US")
    (:united-states-minor-outlying-islands "UM")
    (:uruguay "UY")
    (:uzbekistan "UZ")
    (:vanuatu "VU")
    (:venezuela "VE")
    (:vietnam "VN")
    (:british-virgin-islands "VG")
    (:us-virgin-islands "VI")
    (:wallis-and-futuna "WF")
    (:western-sahara "EH")
    (:yemen "YE")
    (:yugoslavia "YU")
    (:zambia "ZM")
    (:zimbabwe "ZW")))

(defvar *google-languages*
  '((:default "")
    (:english "en")
    (:afrikaans "af")
    (:arabic "ar")
    (:armenian "hy")
    (:belarusian "be")
    (:catalan "ca")
    (:chinese-simplified "zh-CN")
    (:chinese-traditional "zh-TW")
    (:croatian "hr")
    (:czech "cs")
    (:danish "da")
    (:dutch "nl")
    (:esperanto "eo")
    (:estonian "et")
    (:filipino "tl")
    (:finnish "fi")
    (:french "fr")
    (:german "de")
    (:greek "el")
    (:hebrew "iw")
    (:hindi "hi")
    (:hungarian "hu")
    (:icelandic "is")
    (:indonesian "id")
    (:italian "it")
    (:japanese "ja")
    (:korean "ko")
    (:latvian "lv")
    (:lithuanian "lt")
    (:norwegian "no")
    (:persian "fa")
    (:polish "pl")
    (:portuguese "pt")
    (:romanian "ro")
    (:russian "ru")
    (:serbian "sr")
    (:sinhala "si")
    (:slovak "sk")
    (:slovenian "sl")
    (:spanish "es")
    (:swahili "sw")
    (:swedish "sv")
    (:thai "th")
    (:turkish "tr")
    (:ukranian "uk")
    (:vietnamese "vi")
    (:xhosa "xh")
    (:zulu "zu")))

(defun compute-google-lang (code)
  "Returns the corresponding language value from CODE."
  (car (alexandria:assoc-value *google-languages* code :test #'equal)))

(defun compute-edit-google-lang (code)
  "Returns the corresponding language value from CODE and tweaks it."
  (concatenate 'string "lang_" (compute-google-lang code)))

(defun compute-google-country (code)
  "Returns the corresponding country value from CODE."
  (car (alexandria:assoc-value *google-countries* code :test #'equal)))

(defun compute-edit-google-country (code)
  "Returns the corresponding country value from CODE and tweaks it."
  (concatenate 'string "country" (compute-google-country code)))

(define-search-engine google
    (:shortcut "google"
     :fallback-url (quri:uri "https://google.com")
     :base-search-url "https://google.com/search?q=~a"
     :completion-function (make-google-completion)
     :documentation "Google `nyxt:search-engine'.
Does not support advanced results sorting as of now.
Arguments:
SAFE-SEARCH -- Whether results will be filtered. Boolean. t to enable,
nil to disable.
OBJECT -- One of :all :image, :video, :news, :shopping, :books,
:finance.
EXTRA-FILTERS -- Additional search filters.
RESULTS-START -- Displays the search results starting from the given position.
RESULTS-NUMBER -- Number of results to display on a single page.
NEAR-CITY -- Display results near a provided city.
PERSONALIZED-SEARCH -- Whether to show personalized results.
FILETYPE -- Narrow down results to a given file type.
FILETYPE-RULE -- Whether to include or exclude the provided FILETYPE from the results.
SITE -- Narrow down results to a given site.
SITE-RULE - Whether to include or exclude the provided SITE from the results.
EXCLUDE-TERMS -- Removes unwanted whitespace-separated words from the search results.
ACCESS-RIGHTS -- Show results with given license.
NEW-WINDOW -- Open links in a new tab.
FILTER -- Removes the omitted results or similar results filter and allows all
results to be shown.
LANG-RESULTS -- Search results language.
LANG-UI -- Interface language.
COUNTRY-RESULTS -- Use the given country for the search results.
COUNTRY-UI -- Use the given country for the search interface.
COORDINATES -- Search for results near the given coordinates.
DATE-RESULTS -- Filter results by a specified date range.")
  (safe-search "safe" ((t   "strict")
                       (nil "images")))
  (object "tbm" ((:all      "")
                 (:image    "isch")
                 (:video    "vid")
                 (:news     "nws")
                 (:shopping "shop")
                 (:books    "bks")
                 (:finance  "fin")))
  (extra-filters "tbs" ((:sort-by-relevance "")
                        (:sort-by-date "sbd:1")
                        (:archived "ar:1")
                        (:show-duplicates "nsd:1")
                        (:verbatim "li:1")))
  (results-start "start" ((:default 0)))
  (results-number "num" ((:default 10)))
  (near-city "near" ((:default "")))
  (personalized-search "pws" ((t "")
                              (nil "0")))
  (filetype "as_filetype" ((:default "")))
  (filetype-rule "as_ft" ((:include "i")
                          (:exclude "e")))
  (site "as_sitesearch" ((:default "")))
  (site-rule "as_dt" ((:include "i")
                      (:exclude "e")))
  (exclude-terms "as_eq" ((:default "")))
  (access-rights "as_rights" ((:all "")
                              (:cc0 "cc_publicdomain")
                              (:by "cc_attribute")
                              (:by-sa "cc_sharealike")
                              (:by-nc "cc_noncommercial")
                              (:by-nd "cc_nonderived")))
  (new-window "newwindow" ((nil "")
                           (t "1")))
  (filter "filter" ((t "")
                    (nil "0")))
  (lang-results "lr" (:function #'compute-edit-google-lang))
  (lang-ui "hl" (:function #'compute-google-lang))
  (country-results "cr"  (:function #'compute-edit-google-country))
  (country-ui "gl" (:function #'compute-google-country))
  (coordinates "gll" ((:default "")))
  (date-results "as_qdr" ((:default "")
                          (:past-hour "h")
                          (:past-day "d")
                          (:past-week "w")
                          (:past-month "m")
                          (:past-year "y"))))

(define-derived-search-engine google-images
    (google :object :image))
(define-derived-search-engine google-videos
    (google :object :video))
(define-derived-search-engine google-news
    (google :object :news))
(define-derived-search-engine google-shopping
    (google :object :shopping))
(define-derived-search-engine google-reading
    (google :object :books))
(define-derived-search-engine google-finance
    (google :object :finance))

(define-search-engine google-1998
    (:shortcut "g98"
     :base-search-url "https://oldgoogle.neocities.org/search-1998.html?q=hi&num=10#gsc.tab=0&gsc.q=~a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/1998/")
     :documentation "Google-1998 `nyxt:search-engine' for the 1998 version of Google"))
(define-search-engine google-2009
    (:shortcut "g09"
     :base-search-url "https://oldgoogle.neocities.org/2009/search/?hl=en&source=hp&q=~a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2009/")
     :documentation "Google-2009 `nyxt:search-engine' for the 2009 version of Google"))
(define-search-engine google-2010
    (:shortcut "g10"
     :base-search-url "https://oldgoogle.neocities.org/2010/search/?sclient=psy&hl=en&site=webhp&source=hp&q=~a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2010/")
     :documentation "Google-2010 `nyxt:search-engine' for the 2010 version of Google"))
(define-search-engine google-2011
    (:shortcut "g11"
     :base-search-url "https://oldgoogle.neocities.org/2012-search?sclient=psy&hl=en&site=&source=hp&q=a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2011/")
     :documentation "Google-2011 `nyxt:search-engine' for the 2011 version of Google"))
(define-search-engine google-2013
    (:shortcut "g13"
     :base-search-url "https://oldgoogle.neocities.org/2013-search?sclient=psy-ab&site=&source=hp&q=a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2013/")
     :documentation "Google-2013 `nyxt:search-engine' for the 2013 version of Google"))

(defun make-google-scholar-completion (&key request-args)
  (make-search-completion-function
   :base-url "https://scholar.google.com/scholar_complete?q=~a"
   :processing-function
   #'(lambda (results)
       (mapcar (lambda (completion) (remove #\| completion))
               (alexandria:assoc-value (json:decode-json-from-string results) :l)))
   :request-args request-args))

(define-search-engine google-scholar
    (:shortcut "google-scholar"
     :fallback-url (quri:uri "https://scholar.google.com")
     :base-search-url "https://scholar.google.com/scholar?q=~a"
     :completion-function (make-google-scholar-completion)
     :documentation "Google Scholar `nyxt:search-engine'.
Arguments:
STARTING-TIME -- the year since which to search publications.
ENDING-TIME -- the year until which the found publications should span.
SORT-BY -- how to sort the results. Possible values are :RELEVANCE (default) and :DATE.
SEARCH-TYPE -- :ANY for all the papers, :REVIEW to only list review papers.")
  (starting-time "as_ylo" ((:any "")))
  (ending-time "as_yhi" ((:any "")))
  (sort-by "scisbd" ((:relevance "")
                     (:date "1")))
  (search-type "as_rr" ((:any "")
                        (:review "1"))))

(defun make-wikipedia-completion (&key (suggestion-limit 10) (namespace :general) request-args)
  (make-search-completion-function
   :base-url (str:concat "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
                         (format nil "&limit=~d&namespace=~d"
                                 suggestion-limit
                                 (position namespace (list :general :talk
                                                           :user :user-talk
                                                           :wikipedia :wikipedia-talk
                                                           :file :file-talk
                                                           :media-wiki :media-wiki-talk
                                                           :template :template-talk
                                                           :help :help-talk
                                                           :category :category-talk))))
   :processing-function
   #'(lambda (results)
       (when results
         (second (json:decode-json-from-string results))))
   :request-args request-args))

(define-search-engine wikipedia
    (:shortcut "wikipedia"
     :base-search-url "https://en.wikipedia.org/w/index.php?search=~a"
     :fallback-url (quri:uri "https://en.wikipedia.org/")
     :completion-function (make-wikipedia-completion)))

(defun make-yahoo-completion (&key request-args (suggestion-limit 10))
  (make-search-completion-function
   :base-url (str:concat "https://search.yahoo.com/sugg/gossip/gossip-us-ura/?command=~a&output=sd1"
                         (format nil "&nresults=~d" suggestion-limit))
   :processing-function
   #'(lambda (results)
       (when results
         (mapcar #'cdar
                 (alexandria:assoc-value
                  (json:decode-json-from-string
                   (ppcre:regex-replace "YAHOO.*\\(" results ""))
                  :r))))
   :request-args request-args))

(define-search-engine yahoo
    (:shortcut "yahoo"
     :fallback-url (quri:uri "https://search.yahoo.com/")
     :base-search-url "https://search.yahoo.com/search?p=~a"
     :completion-function (make-yahoo-completion)
     :documentation "Yahoo! `nyxt:search-engine'.")
  (number-of-results "n" ((:default "10")))
  (encoding "ei" ((:utf "UTF-8")))
  (domain "vs" ((:any "")
                (:dot-com ".com")
                (:dot-edu ".edu")
                (:dot-gov ".gov")
                (:dot-org ".org")))
  (date "btf" ((:past-day "d")
               (:past-week "d")
               (:past-month "m"))))

(define-search-engine scihub
    (:shortcut "scihub"
     :fallback-url (quri:uri "https://sci-hub.hkvisa.net")
     :base-search-url "https://sci-hub.hkvisa.net/~a"
     :documentation "Sci-Hub `nyxt:search-engine' for research papers"))

(define-search-engine github
    (:shortcut "github"
     :fallback-url (quri:uri "https://github.com/")
     :base-search-url "https://github.com/search?q=~a"
     :documentation "GitHub search engine.
Has no completion, as GitHub doesn't seem to have one.
Use advanced search with

(github :object :advanced)

All the fancy github search params will be there for you.")
  (object "type" ((:repositories "repositories")
                  (:code "code")
                  (:commits "commits")
                  (:issues "issues")
                  (:discussions "discussions")
                  (:packages "registrypackages")
                  (:marketplace "marketplace")
                  (:topics "topics")
                  (:wikis "wikis")
                  (:users "users")
                  (:advanced "advsearch")))
  (language "l" ((:default "")))
  (sort-by "s" ((:best-match "")
                (:stars "stars")
                (:forks "forks")
                (:recently-indexed "indexed")
                (:recently-commited "commiter-date")
                (:recently-authored "author-date")
                (:recently-joined "joined ")
                (:recently-created "created")
                (:recently-updated "updated")
                (:most-commented "comments")
                (:most-downloads "downloads")
                (:most-followers "followers")
                (:most-repositories "repositories")))
  (sort-order "o" ((:descending "desc")
                   (:ascending "asc")))
  ;; Issue-specific
  (state state ((:any "")
                (:open "open")
                (:closed "closed")))
  ;; Package-specific
  (package-type "package_type" ((:any "")
                                (:npm "npm")
                                (:container "container")
                                (:maven "maven")
                                (:nuget "nuget")
                                (:docker "docker")
                                (:rubygems "rubygems"))))

(define-search-engine sourcehut
    (:shortcut "sourcehut"
     :fallback-url (quri:uri "https://sr.ht")
     :base-search-url "https://sr.ht/projects?search=~a"
     :documentation "Sourcehut project search `nyxt:search-engine'.")
  (sort-by "sort" ((:recent "recently-updated")
                   (:active "longest-active"))))

(define-search-engine discourse
    (:shortcut "discourse"
     :fallback-url (quri:uri "https://discourse.atlas.engineer")
     :base-search-url "https://discourse.atlas.engineer/search?q=~a"
     :documentation "`nyxt:search-engine' for Discourse-based instances. You can leverage this engine's
advanced search filters for more precise searches.")
  (search-type "search_type" ((:default "topics/posts")
                              (:categories "categories_tags")
                              (:users "users"))))

(define-search-engine hacker-news
    (:shortcut "hacker-news"
     :fallback-url (quri:uri "https://hn.algolia.com")
     :base-search-url "https://hn.algolia.com/?q=~a"
     :documentation "`nyxt:search-engine' for Hacker News via Algolia Search.")
  (date-range "dateRange" ((:all "all")
                           (:past-day "last24h")
                           (:past-week "pastWeek")
                           (:past-month "pastMonth")
                           (:past-year "pastYear")
                           (:custom "custom")))
  (date-start "dateStart" ((:default "")))
  (date-end "dateEnd" ((:default "")))
  (sort-by "sort" ((:popularity "byPopularity")
                   (:date "byDate")))
  (search-type "type" ((:story "story")
                       (:all "all")
                       (:comment "comment"))))

(define-search-engine lobsters
    (:shortcut "lobsters"
     :fallback-url (quri:uri "https://lobste.rs")
     :base-search-url "https://lobste.rs/search?q=~a"
     :documentation "`nyxt:search-engine' for the computing-focused link-aggregator Lobsters.")
  (search-type "what" ((:default "stories")
                       (:comments "comments")))
  (order-by "order" ((:default "newest")
                     (:relevance "relevance")
                     (:points "points"))))

(define-search-engine nixpkgs
    (:shortcut "nix"
     :base-search-url "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=~a"
     :fallback-url (quri:uri "https://search.nixos.org/")
     :documentation "`nyxt:search-engine' for the package manager nix"))

(define-search-engine openstreetmaps 
    (:shortcut "osm"
     :base-search-url "https://www.openstreetmap.org/search?query=~a"
     :fallback-url (quri:uri "https://www.openstreetmap.org/")
     :documentation "`nyxt:search-engine' for openstreetmaps"))



(define-configuration context-buffer
  ((search-engines (list ;; derived
                         (google-images)
                         (google-videos)
                         (google-news)
                         (google-shopping)
                         (google-reading)
                         (google-finance)
                         (google-scholar)
                         ;; old
                         (google-1998)
                         (google-2009)
                         (google-2010)
                         (google-2011)
                         (google-2013)
                         ;; other
                         (wikipedia)
                         (yahoo)
                         (scihub)
                         (github)
                         (sourcehut)
                         (discourse)
                         (hacker-news)
                         (lobsters)
                         (nixpkgs)
                         (openstreetmaps)
                         ;; default
                         (google :shortcut "g"
                                 :safe-search nil)))))

;;; STATUS

;; ;; quick parenscript to grab out position in the buffer
;; (define-parenscript %percentage ()
;;   (defun percentage ()
;;     (let* ((height-of-window (ps:@ window inner-height))
;;            (content-scrolled (ps:@ window page-y-offset))
;;            (body-height (if (not (or (eql window undefined)
;;                                      (eql (ps:@ window document) undefined)
;;                                      (eql (ps:chain window
;;                                                     document
;;                                                     (get-elements-by-tag-name "body"))
;;                                           undefined)
;;                                      (eql (ps:chain window
;;                                                     document
;;                                                     (get-elements-by-tag-name "body")
;;                                                     0)
;;                                           undefined)
;;                                      (eql (ps:chain window
;;                                                     document
;;                                                     (get-elements-by-tag-name "body")
;;                                                     0
;;                                                     offset-height)
;;                                           undefined)))
;;                           (ps:chain window
;;                                     document
;;                                     (get-elements-by-tag-name "body")
;;                                     0
;;                                     offset-height)
;;                           0))
;;            (total (- body-height height-of-window))
;;            (prc (* (/ content-scrolled total) 100)))
;;       (if (> prc 100)
;;           100
;;           (round prc))))
;;   (percentage))
 
;; (defmethod my-format-status-load-status ((status status-buffer))
;;   "Render the load status to HTML string"theme:on-background-color
;;   (let ((buffer (current-buffer (window status))))
;;     (if (web-buffer-p buffer)
;;         (case (slot-value buffer 'status)
;;           (:loading "∞ ")
;;           (:unloaded "∅ ")
;;           (:finished ""))
;;         "")))

(defmethod my-format-status-url ((status status-buffer))
  "Format the current URL for the STATUS buffer"
  (let* ((buffer (current-buffer (window status)))
         (url-display (multiple-value-bind (aesthetic safe)
                          (render-url (url buffer))
                        (uiop:strcat
                         (if safe
                             (format nil "~a (~a)" safe aesthetic)
                             ;; RFC 2068 says 255 bytes is recommended max, thats 32 characters
                             ;; 62 is the average, so 32 should be ample for the nessecary info
                             (str:prune 32 aesthetic :ellipsis "…"))
                         (when (title buffer)
                           (str:concat " — " (title buffer)))
                         (when (find (url buffer) (remove buffer (buffer-list))
                                     :test #'url-equal :key #'url)
                           (format nil " (buffer ~a)" (id buffer)))))))
    (spinneret:with-html-string
      (:nbutton :buffer status :text url-display :title url-display
        '(nyxt:set-url)))))
 
(defun enabled-modes-string (buffer)
  "Only return enabled modes."
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^~%~}" (mapcar #'princ-to-string (serapeum:filter #'enabled-p (modes buffer))))))

(defmethod my-format-minions ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
        (spinneret:with-html-string
          (:nbutton
            :buffer status
            :text ";-"
            :title (str:concat "Enabled modes: " (enabled-modes-string buffer))
            '(nyxt:toggle-modes)))
    "")))

(defmethod my-format-modes ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
      (str:concat "(" (enabled-modes-string buffer) ")")
      "")))

(defun switch-buffer-or-query-domain (domain)
  (let ((matching-buffers (serapeum:filter (match-domain domain) (buffer-list))))
    (if (eql 1 (length matching-buffers))
        (set-current-buffer (first matching-buffers))
        (switch-buffer-domain :domain domain))))

(defmethod my-format-status-tabs ((status status-buffer))
  "Render the open buffers to HTML string suitable for STATUS."
  (let* ((buffers (if (display-tabs-by-last-access-p status)
                      (sort-by-time (buffer-list))
                      (reverse (buffer-list))))
         (domain-deduplicated-urls (remove-duplicates (mapcar #'url buffers)
                                                      :test #'string=
                                                      :key #'quri:uri-domain)))
    (spinneret:with-html-string
      (loop for url in domain-deduplicated-urls
            collect
            (let* ((domain (quri:uri-domain url))
                   (url url)
                   (current-buffer (current-buffer (window status))))
              (:span
               :id "tab"
               :class (if (string= (quri:uri-domain (url current-buffer))
                                   (quri:uri-domain url))
                          "selected-tab tab"
                          "tab")
               :onclick (ps:ps
                          (if (or (= (ps:chain window event which) 2)
                                  (= (ps:chain window event which) 4))
                              (nyxt/ps:lisp-eval
                               (:title "delete-tab-group"
                                :buffer status)
                               (let ((buffers-to-delete
                                       (serapeum:filter (match-domain domain) buffers)))
                                 (prompt
                                  :prompt "Delete buffer(s)"
                                  :sources (make-instance 'buffer-source
                                                          :constructor buffers-to-delete
                                                          :marks buffers-to-delete
                                                          :actions-on-return (list (lambda-mapped-command buffer-delete))))))
                              (nyxt/ps:lisp-eval
                               (:title "select-tab-group"
                                :buffer status)
                               (switch-buffer-or-query-domain domain))))
               domain))))))

(defmethod format-status ((status status-buffer))
  (let* ((buffer (current-buffer (window status)))
         (buffer-count (1+ (or (position buffer
                                         (sort (buffer-list) #'url-equal :key #'url))
                               0))))
    (spinneret:with-html-string
      (:div :id "container"
            ;; for looks, I should probably make this functional
            (:div :id "vi-mode" "U:**-")
            (:div :id "buffers"
                  (format nil "[~a/~a]"
                      buffer-count
                      (length (buffer-list))))
;;             (:div :id "percentage"
;;                   (format nil "L~a"
;;                       (%percentage)))
             (:div :id "url"
                   (:raw
                    ;;(my-format-status-load-status status)
                    (my-format-status-url status)))
             (:div :id "tabs"
                   (:raw
                    (my-format-status-tabs status)))
             (:div :id "minions"
                   (:raw 
                    (my-format-minions status)))
             (:div :id "modes"
                   (:raw
                     (my-format-modes status)))))))

;;; LOAD

;; simple web-buffer customization
(define-configuration buffer
  (;; basic mode setup for web-buffer
   (default-modes `(,@*buffer-modes*
        ,@%slot-value%))))

;; we wan't to be in insert mode in the prompt buffer, don't show source if theres only one
(define-configuration (prompt-buffer)
  ((default-modes `(vi-insert-mode
         ,@%slot-value%))
   (hide-single-source-header-p t)))
