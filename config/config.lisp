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
      `("li, ul, a"
        :color ,*base04-*
        :font-size "13px")
      `("a code, p code"
        :background-color ,*base02-*
        :color ,*base05-*
        :white-space "pre-wrap"
        :font-size "13px")
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
        :font-family ,*font*
        :font-size "13px")
      `(body
        :background-color ,*base00-*
        :margin "0")
      '("#root"
        :height "100%"
        :display "grid"
        :grid-template-rows "auto 1fr")
      `("#prompt-area"
        :margin "4px"
        :background-color ,*base01-*
        :color ,*base05-*
        :display "grid"
        :grid-template-columns "auto auto 1fr auto auto")
      `("#prompt"
        :background-color ,*base01-*
        :color ,*base05-*
        :padding-left "10px"
        :line-height "28px"
        :max-width "40ch"
        :overflow "hidden"
        :white-space "nowrap"
        :text-overflow "ellipsis")
      '("#prompt-input"
        :min-width "10ch"
        :line-height "28px"
        :padding-right "10px")
      `("#prompt-extra"
        :font-family ,*mono*
        :min-width "12px"
        :background-color ,*base01-*
        :color ,*base05-*
        :line-height "28px"
        :padding-right "5px")
      `("#prompt-modes"
        :line-height "28px"
        :padding-left "3px"
        :padding-right "3px")
      `("#close-button"
        :text-align "right"
        :padding-right "7px"
        :background-color ,*base01-*
        :min-width "24px"
        :line-height "28px"
        :font-weight "bold"
        :font-size "20px")
      '(button
        :background "transparent"
        :color "inherit"
        :text-decoration "none"
        :border "none"
        :padding 0
        :font "inherit"
        :outline "inherit")
      `(.button.action
        :background-color ,*base0C-*
        :color ,*base04-*)
      `((:and .button :hover)
        :cursor "pointer"
        :color ,*base06-*)
      `(".button:hover svg path"
        :stroke ,*base0C-*)
      `((:and .button (:or :visited :active))
        :color ,*base00-*)
      `(input
        :font-family ,*mono*)
      `("#input"
        :height "28px"
        :background-color ,*base01-*
        :color ,*base05-*
        :outline "none"
        :width "100%"
        :autofocus "true")
      '(".source"
        :margin-left "10px"
        :margin-top "15px")
      `(".source-name"
        :padding-left "4px"
        :background-color ,*base08-*
        :color ,*base04-*
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
      '("#next-source > svg, #previous-source > svg"
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
         :color ,*base04-*
         :cursor "pointer")
        (th
         :background-color ,*base0E-*
         :color ,*base04-*
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

;;; COMMANDS

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

;; open markdown preview in a split
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

(define-command-global open-terminal ()
  "Open a terminal in a new buffer"
  (let ((term-buffer (make-buffer :title "*term*"
                                  :url "http://localhost:7681/"
                                  :modes 'nyxt/mode/passthrough:passthrough-mode)))
    (set-current-buffer term-buffer)))

(define-panel-command-global open-terminal-split ()
    (panel "*term split*" :right)
  "Open a terminal on the right buffer"
  (run-thread "term loader"
    (setf 
      (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (sleep 0.3)
    (buffer-load (quri:uri "http://localhost:7681/")
                 :buffer panel))
  "")

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
