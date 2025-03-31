(in-package #:nyxt-user)

(defun dot-product-recursive (a b)
  "Calculate dot product recursively"
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product-recursive (rest a) (rest b)))))

(defun dot-product (list1 list2)
  "Calculate dot product iteratively"
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do (setf sum (+ sum (* x y))))
    sum))

;;; this file was created and edited in NYXT with ace-mode

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

;; style config
(define-nyxt-user-system-and-load nyxt-user/style-config
  :components ("style"
               "status"))

;;(define-nyxt-user-system-and-load nyxt-user/mode-config
;;  :components ("zotero"
;;               "ace"))

(define-nyxt-user-system-and-load nyxt-user/extra-config
  :components ("startpage"
               "commands"
	       "hardware"
	       "launcher"
	       "wayland"
	       "zola"
               "tor"
               "mpv"
               "ace"
               ;;"repl"
               "ai"
               "search-engines"
               "fetch"))

;;(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/bookmark-mode:bookmarks-file))
;;  #p"bookmarks.lisp")
;;(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/no-procrastinate-mode:no-procrastinate-hosts-file))
;;  #p"no-procrastinate-hosts")
;;(defmethod files:resolve ((profile nyxt:nyxt-profile) (file auto-rules-file))
;;  #p"auto-rules.lisp")

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

;; show startuptime 
;; (define-configuration browser
;;   ((before-initialize-hook
;;     (lambda (browser)
;;       (setf (gethash 'start-time (session-data browser)) (get-internal-real-time)))))
;; 
;;   ((after-initialize-hook
;;     (lambda (browser)
;;       (let* ((start-time (gethash 'start-time (session-data browser)))
;;              (end-time (get-internal-real-time))
;;              (ticks-per-second internal-time-units-per-second)
;;              (startup-time (/ (- end-time start-time) ticks-per-second)))
;;         (echo (format nil "Startup time: ~,3f seconds" startup-time)))))))
