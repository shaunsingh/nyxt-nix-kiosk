(in-package #:nyxt-user)

;; load quicklisp if it's available
;; #-quicklisp
;; (let ((quicklisp-init
;;        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))

;;; MODES

(defvar *buffer-modes*
  '(vi-normal-mode
    dark-mode)
 "Modes to enable in buffer by default")

;;; STYLE

;; ;;;; quick parenscript to grab out position in the buffer
;; ;; (define-parenscript %percentage ()
;; ;;   (defun percentage ()
;; ;;     (let* ((height-of-window (ps:@ window inner-height))
;; ;;            (content-scrolled (ps:@ window page-y-offset))
;; ;;            (body-height (if (not (or (eql window undefined)
;; ;;                                      (eql (ps:@ window document) undefined)
;; ;;                                      (eql (ps:chain window
;; ;;                                                     document
;; ;;                                                     (get-elements-by-tag-name "body"))
;; ;;                                           undefined)
;; ;;                                      (eql (ps:chain window
;; ;;                                                     document
;; ;;                                                     (get-elements-by-tag-name "body")
;; ;;                                                     0)
;; ;;                                           undefined)
;; ;;                                      (eql (ps:chain window
;; ;;                                                     document
;; ;;                                                     (get-elements-by-tag-name "body")
;; ;;                                                     0
;; ;;                                                     offset-height)
;; ;;                                           undefined)))
;; ;;                           (ps:chain window
;; ;;                                     document
;; ;;                                     (get-elements-by-tag-name "body")
;; ;;                                     0
;; ;;                                     offset-height)
;; ;;                           0))
;; ;;            (total (- body-height height-of-window))
;; ;;            (prc (* (/ content-scrolled total) 100)))
;; ;;       (if (> prc 100)
;; ;;           100
;; ;;           (round prc))))
;; ;;   (percentage))
;; 
;; ;; statusbar components
;; (defmethod my-format-status-load-status ((status status-buffer))
;;   "Render the load status to HTML string"
;;   (let ((buffer (current-buffer (window status))))
;;     (if (web-buffer-p buffer)
;;         (case (slot-value buffer 'status)
;;           (:loading "∞ ")
;;           (:unloaded "∅ ")
;;           (:finished ""))
;;         "")))
;; 
;; (defmethod my-format-status-url ((status status-buffer))
;;   "Format the current URL for the STATUS buffer"
;;   (let* ((buffer (current-buffer (window status)))
;;          (url-display (multiple-value-bind (aesthetic safe)
;;                           (render-url (url buffer))
;;                         (uiop:strcat
;;                          (if safe
;;                              (format nil "~a (~a)" safe aesthetic)
;;                              ;; RFC 2068 says 255 bytes is recommended max, thats 32 characters
;;                              ;; 62 is the average, so 32 should be ample for the nessecary info
;;                              (str:prune 32 aesthetic :ellipsis "…"))
;;                          (when (title buffer)
;;                            (str:concat " — " (title buffer)))
;;                          (when (find (url buffer) (remove buffer (buffer-list))
;;                                      :test #'url-equal :key #'url)
;;                            (format nil " (buffer ~a)" (id buffer)))))))
;;     (spinneret:with-html-string
;;       (:nbutton :buffer status :text url-display :title url-display
;;         '(nyxt:set-url)))))
;; 
;; (defun modes-string (buffer)
;;   (when (modable-buffer-p buffer)
;;     (format nil "~{~a~^~%~}" (append '("Enabled modes:")
;;                                      (mapcar #'princ-to-string (enabled-modes buffer))))))
;; 
;; (defmethod my-format-minions ((status status-buffer))
;;   (let ((buffer (current-buffer (window status))))
;;     (if (modable-buffer-p buffer)
;;         (spinneret:with-html-string
;;           (:nbutton
;;             :buffer status
;;             :text ";-"
;;             :title (str:concat "Enabled modes: " (modes-string buffer))
;;             '(nyxt:toggle-modes)))
;;     "")))
;; 
;; (defmethod format-status ((status status-buffer))
;;   (let* ((buffer (current-buffer (window status)))
;;          (buffer-count (1+ (or (position buffer
;;                                          (sort (buffer-list) #'url-equal :key #'url))
;;                                0))))
;;     (spinneret:with-html-string
;;       (:div :id "container"
;;             ;; for looks, I should probably make this functional
;;             (:div :id "vi-mode" "U:**-"))
;;             ;; buffer count
;;             (:div :id "buffers"
;;                   (format nil "[~a/~a]"
;;                       buffer-count
;;                       (length (buffer-list))))
;;             ;; scroll length
;;             ;; (:div :id "percentage"
;;             ;;       (format nil "L~a"
;;             ;;           (%percentage)))
;;             ;; url/loading
;;             (:div :id "url"
;;                   (:raw
;;                    ;;(my-format-status-load-status status)
;;                    (my-format-status-url status)))
;;             ;; emacs-esque minions
;;             (:div :id "minions"
;;                   (:raw 
;;                    (my-format-minions status)))
;;             ;; show open tabs
;;             (:div :id "tabs"
;;                   (:raw
;;                    (format-status-tabs status)))
;;             ;; (:div :id "modes"
;;             ;;       (:raw
;;             ;;        "("
;;             ;;        (modes-string buffer)
;;             ;;        ")"))
;;             )))

;; oxocarbon colorscheme
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

;; (define-configuration nyxt/mode/style:dark-mode
;;   ((style
;;     (theme:themed-css (theme *browser*)
;;       `(body
;;         :background-color ,*base00-*
;;         :color ,*base04-*
;;         :font-family ,*font*
;;         :margin "4% 6%")
;;       `("#container"
;;         :white-space "nowrap"
;;         :overflow "hidden")
;;       `("h1, #subtitle"
;;         :font-size "63px"
;;         :margin-bottom "-9px")
;;       `("h1"
;;         :color ,(make-important *base06-*))
;;       `("#subtitle"
;;         :color ,(make-important *base0C-*))
;;       `("h2"
;;         :font-size "27px"
;;         :margin-bottom "-5px"
;;         :color ,(make-important *base0C-*))
;;       `("h3"
;;         :font-size "18px"
;;         :margin-bottom "-3px"
;;         :color ,(make-important *base0E-*))
;;       `("h4"
;;         :color ,(make-important *base0F-*))
;;       `("h5"
;;         :color ,(make-important *base09-*))
;;      `("hr"
;;        :background-color ,*base01-*
;;        :color ,*base04-*
;;        :border-radius "0")
;;      `("p, li, ul, a"
;;        :font-size "13px")
;;      `("a"
;;        :white-space "pre-wrap")
;;      `(".button"
;;        :background-color ,*base01-*
;;        :background-color ,*base04-*
;;        :border-radius "0")
;; ))))

;; (define-configuration :status-buffer
;;   ((height 36)
;;    (style
;;     (theme:themed-css (theme *browser*)
;;       ;; by default nyxt sets a proportional font
;;       `(*
;;         :font-family ,*mono*
;;         :font-size "11px")
;;       ;; add some padding around the body
;;       `(body
;;         :margin "9px"
;;         :background-color *base01-* 
;;         :color *base05-*)
;;       ;; let the statusline overflow
;;       `("#container"
;;         ;;:display "flex"
;;         :white-space "nowrap"
;;         :overflow "hidden")
;;       ;; add a generous amount of padding around everything
;;       `("#vi-mode, #buffers, #percentage, #url, #minions, #tabs, #modes"
;;         :padding-left "9px")
;;       ;; url can be nice and bright
;;       `("#url"
;;         :color *base06-* 
;;         :font-weight "bold")
;;       ;; modes can be dull and dark
;;       `("#modes"
;;         :color "#a2a9b0")
;;       ;; button tweaks incl vim color
;;       `(button
;;         :all "unset"
;;         :color *base04-*)
;;       `((:and (:or .button .tab "#url") :hover)
;;         :font-weight "bold"
;;         :cursor "pointer")))))

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

