(in-package #:nyxt-user)

;; load quicklisp if it's available
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; MODES

(defvar *buffer-modes*
  '(vi-normal-mode
    dark-mode)
 "Modes to enable in buffer by default")

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
 
(defmethod format-status ((status status-buffer))
  (let* ((buffer (current-buffer (window status)))
         (buffer-count (1+ (or (position buffer
                                         (sort (buffer-list) #'url-equal :key #'url))
                               0))))
    (spinneret:with-html-string
      (:div :id "container"
            ;; for looks, I should probably make this functional
            (:div :id "vi-mode" "U:**-")
            ;; buffer count
            (:div :id "buffers"
                  (format nil "[~a/~a]"
                      buffer-count
                      (length (buffer-list))))
;;            ;; scroll length
;;             (:div :id "percentage"
;;                   (format nil "L~a"
;;                       (%percentage)))
             ;; load status
             ;;(:div :id "load"
             ;;      (:raw
             ;;       (my-format-status-load-status status)))
             ;; url
             (:div :id "url"
                   (:raw
                    (my-format-status-url status)))
             ;; emacs-esque minions
             (:div :id "minions"
                   (:raw 
                    (my-format-minions status)))
             ;; show open tabs
             (:div :id "tabs"
                   (:raw
                    (format-status-tabs status)))
             (:div :id "modes"
                   (:raw
		     (my-format-modes status)))))))

;;; COLORSCHEME

(defmacro define-palette (&rest colors)
  "Helper macro to set global variables for `theme' colors"
  `(progn ,@(loop for (name hex)
                  in colors
                  collect `(defparameter ,name ,hex "Color used for `theme'"))))

(defun make-important (property)
  (str:concat property " !important"))

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

(define-configuration nyxt/mode/style:dark-mode
  ((style
    (theme:themed-css (theme *browser*) 
      `(body
        :background-color ,*base00-*
        :color ,*base04-*
        :font-family ,*font*
        :margin "4% 6%")
      `("h1, #subtitle"
        :font-size "63px"
        :margin-bottom "-9px")
      `("h1"
        :color ,(make-important *base06-*))
      `("#subtitle"
        :color ,(make-important *base0C-*))
      `("h2"
        :font-size "27px"
        :margin-bottom "-2px"
        :color ,(make-important *base0C-*))
      `("h3"
        :font-size "18px"
        :margin-bottom "-2px"
        :color ,(make-important *base0E-*))
      `("h4"
        :color ,(make-important *base0F-*))
      `("h5"
        :color ,(make-important *base09-*))
      `("a"
        :color ,*base06-*)
      `("li, ul, pre"
        :color ,*base05-*)
      `("a code, p code, pre, code"
        :font-family ,*mono*
        :background-color ,*base01-*
        :color ,*base06-*
        :white-space "pre-wrap")
      `("hr, .button"
        :background-color ,(make-important *base01-*)
        :border-color ,(make-important *base01-*)
        :color ,(make-important *base04-*)
        :border-radius "0")))))

(define-configuration :status-buffer
  ((height 36)
   (style
    (theme:themed-css (theme *browser*)
      ;; by default nyxt sets a proportional font
      `(*
        :font-family ,*mono*
        :font-size "11px")
      ;; add some padding around the body
      `(body
        :margin "9px"
	:margin-top "11px"
        :background-color ,*base02-* 
        :color ,*base05-*)
      ;; let the statusline overflow
      `("#container"
        :display "flex"
        :white-space "nowrap"
        :overflow "hidden")
      ;; add a generous amount of padding around everything
      `("#vi-mode, #buffers, #load, #percentage, #url, #minions, #tabs, #modes"
        :padding-left "9px")
      ;; url can be nice and bright
      `("#url"
        :color ,*base06-* 
        :font-weight "bold")
      ;; modes can be dull and dark
      `("#modes"
        :color "#a2a9b0")
      ;; button tweaks incl vim color
      `(button
        :all "unset"
        :color ,*base04-*)
      `((:and (:or .button .tab "#url") :hover)
        :font-weight "bold"
        :cursor "pointer")))))

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
        :max-width "40ch"
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
         :color ,*base06-*
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
        :color ,*base06-*
        :font-weight "bold")
      `(.marked
        :background-color ,*base0B-*
        :color ,*base06-*
        :font-weight "bold")
      `(.selected
        :background-color ,*base06-*
        :color ,*base00-*)))))

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

;; ;;; STARTPAGE
;; 
;; (defparameter list-of-fruits
;;   (list "abiu"
;;         "açaí"
;;         "acerola"
;;         "ackee"
;;         "african cucumber"
;;         "apple"
;;         "apricot"
;;         "avocado"
;;         "banana"
;;         "bilberry"
;;         "blackberry"
;;         "blackcurrant"
;;         "black sapote"
;;         "blueberry"
;;         "boysenberry"
;;         "breadfruit"
;;         "buddha's hand (fingered citron)"
;;         "cactus pear"
;;         "canistel"
;;         "cempedak"
;;         "cherimoya (Custard Apple)"
;;         "cherry"
;;         "chico fruit"
;;         "cloudberry"
;;         "coco De Mer"
;;         "coconut"
;;         "crab apple"
;;         "cranberry"
;;         "currant"
;;         "damson"
;;         "date"
;;         "dragonfruit (or Pitaya)"
;;         "durian"
;;         "egg Fruit"
;;         "elderberry"
;;         "feijoa"
;;         "fig"
;;         "finger Lime (or Caviar Lime)"
;;         "goji berry"
;;         "gooseberry"
;;         "grape"
;;         "raisin"
;;         "grapefruit"
;;         "grewia asiatica (phalsa or falsa)"
;;         "guava"
;;         "hala Fruit"
;;         "honeyberry"
;;         "huckleberry"
;;         "jabuticaba"
;;         "jackfruit"
;;         "jambul"
;;         "japanese plum"
;;         "jostaberry"
;;         "jujube"
;;         "juniper berry"
;;         "kaffir Lime"
;;         "kiwano (horned melon)"
;;         "kiwifruit"
;;         "kumquat"
;;         "lemon"
;;         "lime"
;;         "loganberry"
;;         "longan"
;;         "loquat"
;;         "lulo"
;;         "lychee"
;;         "magellan Barberry"
;;         "mamey Apple"
;;         "mamey Sapote"
;;         "mango"
;;         "mangosteen"
;;         "marionberry"
;;         "melon"
;;         "cantaloupe"
;;         "galia melon"
;;         "honeydew"
;;         "mouse melon"
;;         "musk melon"
;;         "watermelon"
;;         "miracle fruit"
;;         "monstera deliciosa"
;;         "mulberry"
;;         "nance"
;;         "nectarine"
;;         "orange"
;;         "blood orange"
;;         "clementine"
;;         "mandarine"
;;         "tangerine"
;;         "papaya"
;;         "passionfruit"
;;         "pawpaw"
;;         "peach"
;;         "pear"
;;         "persimmon"
;;         "plantain"
;;         "plum"
;;         "prune (dried plum)"
;;         "pineapple"
;;         "pineberry"
;;         "plumcot (or Pluot)"
;;         "pomegranate"
;;         "pomelo"
;;         "purple mangosteen"
;;         "quince"
;;         "raspberry"
;;         "salmonberry"
;;         "rambutan (or Mamin Chino)"
;;         "redcurrant"
;;         "rose apple"
;;         "salal berry"
;;         "salak"
;;         "satsuma"
;;         "shine Muscat or Vitis Vinifera"
;;         "sloe or Hawthorn Berry"
;;         "soursop"
;;         "star apple"
;;         "star fruit"
;;         "strawberry"
;;         "surinam cherry"
;;         "tamarillo"
;;         "tamarind"
;;         "tangelo"
;;         "tayberry"
;;         "ugli fruit"
;;         "white currant"
;;         "white sapote"
;;         "yuzu"
;;         "bell pepper"
;;         "chile pepper"
;;         "corn kernel"
;;         "cucumber"
;;         "eggplant"
;;         "jalapeño"
;;         "olive"
;;         "pea"
;;         "pumpkin"
;;         "squash"
;;         "tomato"
;;         "zucchini"))
;; 
;; ;; nice words
;; (defparameter list-of-pretty-words
;;   (list "lovely"
;;         "wonderful"
;;         "delightful"
;;         "beautiful"
;;         "pleasant"
;;         "adorable"
;;         "sweet"
;;         "delicious"
;;         "charming"
;;         "fantastic"
;;         "gorgeous"
;;         "heavenly"
;;         "magnificent"
;;         "radiant"
;;         "splendid"
;;         "exquisite"
;;         "enchanting"
;;         "serene"
;;         "blissful"
;;         "harmonious"
;;         "majestic"
;;         "tranquil"
;;         "whimsical"
;;         "ethereal"
;;         "celestial"
;;         "idyllic"
;;         "mesmerizing"
;;         "spellbinding"
;;         "captivating"
;;         "fascinating"
;;         "riveting"
;;         "enthralling"
;;         "mesmerizing"
;;         "inspiring"))
;; 
;; ;; have some alliteration word fun
;; (defun fruit-of-the-day-message ()
;;   (flet ((capitalize-word (word)
;;            (concatenate 'string (string-upcase (subseq word 0 1))
;;                               (subseq word 1))))
;;     (let* ((current-time (local-time:now))
;;            (current-day (aref local-time:+day-names+
;;                               (local-time:timestamp-day-of-week current-time)))
;;            (current-fruit (nth (mod (local-time:day-of current-time)
;;                                     (length list-of-fruits))
;;                                list-of-fruits))
;;            (matching-words (remove-if-not (lambda (word)
;;                                             (char= (char word 0)
;;                                                    (char current-fruit 0)))
;;                                           list-of-pretty-words))
;;            (word (if matching-words
;;                      (nth (random (length matching-words)) matching-words)
;;                      (nth (random (length list-of-pretty-words))
;;                           list-of-pretty-words))))
;;       (format nil "Have ~A ~A ~A ~A!"
;;               (if (member (char (string word) 0) '(#\a #\e #\i #\o #\u))
;;                   "an" "a")
;;               (capitalize-word word)
;;               (capitalize-word current-fruit)
;;               current-day))))
;; 
;; ;; stolen from time.lisp
;; (defun sort-by-time (sequence &key (key #'last-access))
;;   "Return a timely ordered SEQUENCE by KEY.  More recent elements come first."
;;   (sort sequence #'local-time:timestamp> :key key))
;; 
;; ;; now to bring it all together
;; (define-internal-page-command-global startpage ()
;;     (buffer "*startpage*")
;;   "my custom startpage"
;;   (flet ((list-bookmarks (&key (limit 6) (separator " → "))
;;            (spinneret:with-html-string
;;              (let ((mode (make-instance 'nyxt/bookmark-mode:bookmark-mode)))
;;                (alexandria:if-let ((bookmarks (files:content (nyxt/bookmark-mode:bookmarks-file mode))))
;;                  (dolist (bookmark (serapeum:take limit (the list (sort-by-time bookmarks :key #'nyxt/bookmark-mode:date))))
;;                    (:li (title bookmark) separator
;;                         (:a :href (render-url (url bookmark))
;;                             (render-url (url bookmark)))))
;;                  (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/bookmark-mode:bookmarks-file mode)))))))))
;;     (let ((current-year (local-time:timestamp-year (local-time:now)))
;;           (dashboard-style (theme:themed-css (theme *browser*)
;;                               `("#motto"
;;                                 :font-size "27px"
;;                                 :margin"18px"
;;                                 :margin-left "3px"
;;                                 :color ,*base08*)
;;                               `("#copyright"
;;                                 :position "absolute"
;;                                 :text-align "right"
;;                                 :bottom "1.5em"
;;                                 :right "1.5em"))))
;;      (spinneret:with-html-string
;;        (:nstyle dashboard-style)
;;        (:div :id "container"
;;         (:h1 "Welcome to " (:span :id "subtitle" "NYXT ☺"))
;;         (:div :id "buttons"
;;          (:nbutton :text "Restore Session"
;;            (nyxt::restore-history-by-name))
;;          (:nbutton :text "Open Repl"
;;            (nyxt/repl-mode:repl))
;;          (:nbutton :text "View Changelog"
;;            (nyxt:changelog))
;;          (:nbutton :text "View Bookmarks"
;;            (nyxt/bookmark-mode:list-bookmarks))
;;          (:nbutton :text "View Annotations"
;;            (nyxt/annotate-mode:show-annotations)))
;;         (:div :id "motto"
;;          "私たちのミッションは"
;;          (:br)
;;          "先端工学を用いて上質で"
;;          (:br)
;;          "機能的なデザインの"
;;          (:br)
;;          "製品を作り出すことです。")
;;         (:h2 "Recents")
;;         (:h3 "Bookmarks")
;;         (:ul (:raw (list-bookmarks :limit 9)))
;;         (:h3 "History")
;;         (:ul (:raw (nyxt::history-html-list :limit 9)))
;;         (:h2 (fruit-of-the-day-message))
;;         (:div :id "copyright"
;;           (format nil "version ~a ~a" (name nyxt::*renderer*) nyxt::+version+)
;;           (:br)
;;           (format nil "lisp ~a ~a" (lisp-implementation-type) (lisp-implementation-version))
;;           (:br)
;;           (format nil "host ~a@~a" (software-type) (software-version))
;;           (:br)
;;           (format nil "Atlas Engineer LLC, 2018-~a" current-year)
;;           (:br)
;;           (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+)))))))
;; 
;; ;; set default url to startpage
;; (define-configuration browser
;;   ((default-new-buffer-url (quri:uri "nyxt:nyxt-user:startpage"))))

;;; COMMANDS

(define-panel-command-global vsplit (&key (url (quri:render-uri (url (current-buffer)))))
    (panel "*Duplicate panel*" :right)
  "Duplicate the current buffer URL in the panel buffer on the right.

A poor man's vsplit :("
  (setf 
    (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
  (run-thread "URL loader"
    (sleep 0.3)
    (buffer-load (quri:uri url) :buffer panel))
  "")

(define-panel-command-global search-translate-selection (&key (selection (ffi-buffer-copy (current-buffer))))
    (panel "*Translate panel*" :right)
  "Open the translation of the selected word in a panel buffer."
  (run-thread "search translation URL loader"
    (setf
      (ffi-width panel) (round (/ (ffi-width (current-window)) 3)))
    (sleep 0.3)
    (buffer-load (quri:uri (format nil (nyxt::search-url (nyxt::default-search-engine))
                                   (str:concat "translate " (ffi-buffer-copy (current-buffer)) "to english")))
                 :buffer panel))
  "")

(ffi-add-context-menu-command
 'search-translate-selection
 "Translate Selection")

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

(define-panel-command open-preview ()
    (panel "*markdown preview*" :right)
  "Open a file to preview using grip on the right buffer"
  (run-thread "grip loader"
    (setf
      (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (sleep 0.3)
    (buffer-load (quri:uri "http://localhost:6419")
                 :buffer panel))
  "")

(define-command-global open-markdown (&key (file (my-prompt-for-file)))
  "Open a markdown file with a grip-powered preview."
  (flet ((launch-grip (file-path)
           (uiop:launch-program (format nil "grip ~a" file-path))))
    (let ((buffer (make-instance 'nyxt/mode/editor:editor-buffer
                                 :url (quri:make-uri :scheme "editor" :path file))))
      (set-current-buffer buffer)
      (launch-grip file)
      (open-preview))))

(define-command-global close-preview ()
  "Close grip preview window"
  (delete-all-panel-buffers)
  (uiop:launch-program "pkill grip"))

(define-panel-command-global zola-preview () 
  (panel "*zola preview*" :right) 
  "Open the Zola preview of the current markdown file on the right buffer"
  (run-thread "zola preview loader" 
    (setf (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (sleep 0.3)
    (buffer-load (quri:uri "http://localhost:1111") :buffer panel))
  "")

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

(define-command-global edit-and-preview-with-zola (&key (file (my-prompt-for-file)))
  "Open a markdown file with editor and start Zola preview if possible."
  (let ((buffer (make-instance 'nyxt/mode/editor:editor-buffer 
                               :url (quri:make-uri :scheme "editor" :path file)))
        (zola-dir (find-zola-config-directory file)))
    (set-current-buffer buffer)
    (if zola-dir
        (progn
          (run-zola-serve zola-dir)
          (zola-preview)
          (echo "Zola preview started for directory: ~a" zola-dir))
        (echo "No Zola config.toml found in parent directories"))))

(define-command-global close-zola-preview ()
  "Close Zola preview window and stop Zola server"
  (delete-all-panel-buffers)
  (uiop:launch-program "pkill zola"))

;;; SYSTEM

(define-command-global screenshot ()
  "Take a screenshot"
  (uiop:launch-program "grim"))

(define-command-global screenshot-to-clipboard ()
  "Take a screenshot to clipboard"
  (uiop:launch-program "grim - | wl-copy"))

(define-command-global screenshot-region ()
  "Take a screenshot of a region and copy to clipboard"
  (uiop:launch-program "grim -g \"$(slurp)\" - -t png | wl-copy -t image/png"))

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

(define-mode ace-mode (nyxt/mode/editor:editor-mode nyxt/mode/passthrough:passthrough-mode)
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
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.15.3/ace.min.js"
      :crossorigin "anonymous"
      :type "text/javascript"
      :charset "utf-8"
      "")
     (:script
      :src "https://www.unpkg.com/ace-linters@0.6.0/build/ace-linters.js"
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
  ((style (str:concat
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
               :background-color ,*base0a-*)
              `(".oxocarbon .ace_marker-layer .ace_active-line"
               :background-color ,*base01-*)
              `(".oxocarbon .ace_gutter-active-line"
               :color ,*base06-*)
              `(".oxocarbon .ace_marker-layer .ace_selected-word"
               :background-color ,*base02-*)
              `(".oxocarbon .ace_fold"
               :color ,*base04-*)
              ;; token styles
              `(".oxocarbon .ace_comment"
               :color ,*base03-*)
              `(".oxocarbon .ace_keyword"
               :color ,*base0c-*)
              `(".oxocarbon .ace_constant.ace_numeric"
               :color ,*base0f-*)
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
               :color ,*base0c-*)
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
               :color ,*base0a-*)
              `(".oxocarbon .ace_invalid.ace_deprecated"
               :color ,*base03-*)
              `(".oxocarbon .ace_string"
               :color ,*base0e-*)
              `(".oxocarbon .ace_variable"
               :color ,*base0f-*)
              `(".oxocarbon .ace_variable.ace_parameter"
               :color ,*base04-*)
              `(".oxocarbon .ace_entity.ace_other.ace_attribute-name"
               :color ,*base0b-*)
              `(".oxocarbon .ace_entity.ace_name.ace_tag"
               :color ,*base0f-*)
              `(".oxocarbon .ace_invisible"
               :color ,*base03-*))))
   (:keybindings "ace/keyboard/vim")
   (extensions
     (mapcar
       (lambda (name)
         (quri:merge-uris (quri:uri name)
        (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.15.3/")))
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
         "ext-searchbox.min.js"        ;; used for cmd/ctrl-f dialogue
         "ext-whitespace.min.js"       ;; detect spacing/indent
         ;;"ext-split.min.js"            ;; enable split functionality
         "ext-settings_menu.min.js"    ;; view and adjust settings
         "ext-keybinding_menu.min.js"  ;; view and adjust keybindings
         "ext-modelist.min.js"         ;; detect mode based on filepath
         "ext-beautify.min.js")))      ;; formatting support
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
           (req "ace/ext/searchbox")
           (req "ace/ext/whitespace")
           (req "ace/ext/split")
           (ps:chain (req "ace/ext/settings_menu") (init editor))
           (ps:chain (req "ace/ext/keybinding_menu") (init editor))
           (ps:chain editor session
                     (set-mode (ps:chain (req "ace/ext/modelist")
                                         (get-mode-for-path (ps:@ window location href)) mode)))
           (ps:chain editor commands
                     (add-command (ps:chain ace (require "ace/ext/beautify") commands 0)))
           ;; editor configpassthrough-mode
           (ps:chain editor (set-option "cursorstyle" "wide"))          ;; static cursor
           (ps:chain editor (set-option "readonly" nil))                ;; set read and write file
           (ps:chain editor (set-option "fontsize" 15))                 ;; bigger default font
           (ps:chain editor (set-option "showlinenumbers" nil))         ;; disable line numbers
           (ps:chain editor (set-option "showprintmargin" t))           ;; enable print margin (colorline)
           (ps:chain editor (set-option "displayindentguides" nil))     ;; disable indent markers
           (ps:chain editor (set-option "hscrollbaralwaysvisible" nil)) ;; don't always show scrollbar (h)
           (ps:chain editor (set-option "vscrollbaralwaysvisible" nil)) ;; don't always show scrollbar (v)
           (ps:chain editor (set-option "usesofttabs" t))               ;; use spaces instead of tabs
           (ps:chain editor (set-option "enablesnippets" t))            ;; enable snippet support
           (ps:chain editor (set-option "highlightactiveline" t))       ;; highlight current line
           (ps:chain editor (set-option "enablebasicautocompletion" t)) ;; enable (basic) autocompleetion
           ;; vim bindings
           (vi-noremap "j" "gj" "normal")
           (vi-noremap "k" "gk" "normal")
           ;; vim ex commands
           (vi-define-ex "write" "w" (lambda (cm input)
                                        (ps:chain cm ace (exec-command "save"))))
           (vi-define-ex "help" "h" (lambda ()
                                        (ps:chain editor (show-keyboard-shortcuts))))
           (vi-define-ex "settings" "se" (lambda ()
                                            (ps:chain editor (show-settings-menu))))
           ;; load workers
           (req "ace/worker/base")
           ;; register ace linters
           (ps:chain -language-provider (from-cdn "https://www.unpkg.com/ace-linters@0.6.0/build/")
                     (register-editor editor))))))))

;; use ace for editor-mode by default
(define-configuration nyxt/mode/editor:editor-buffer
  ((default-modes `(ace-mode ,@%slot-value%))))

;;; LOAD

;; simple web-buffer customization
(define-configuration buffer
  (;; basic mode setup for web-buffer
   (default-modes `(,@*buffer-modes*
        ,@%slot-value%))))

;; we wan't to be in insert mode in the prompt buffer
(define-configuration (prompt-buffer)
  ((default-modes `(vi-insert-mode
         ,@%slot-value%))))

;;; WEBKIT

;; this was mostly guessing, thouogh this link provides basic docs for the options
;; https://webkitgtk.org/reference/webkit2gtk/stable/class.Settings.html#properties
(defmethod ffi-buffer-make :after ((buffer buffer))
  (let* ((settings (webkit:webkit-web-view-get-settings
                    (nyxt/renderer/gtk::gtk-object buffer))))
    (setf
     ;; enable encrypted (DRM) content if possible
     (webkit:webkit-settings-enable-encrypted-media settings) t
     ;; enable resizable text areas
     (webkit:webkit-settings-enable-resizable-text-areas settings) t
     ;; enable inspect
     (webkit:webkit-settings-enable-developer-extras settings) t
     ;; enable webgl support
     (webkit:webkit-settings-enable-webgl settings) t
     ;; use SF Pro Text as proportional font
     (webkit:webkit-settings-default-font-family settings) "SF Pro Text"
     (webkit:webkit-settings-default-font-size settings) 15
     ;; use Liga SFMono Nerd Font as monospace font
     (webkit:webkit-settings-monospace-font-family settings) "Liga SFMono Nerd Font"
     (webkit:webkit-settings-default-monospace-font-size settings) 13)))
