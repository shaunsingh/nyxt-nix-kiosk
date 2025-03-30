(in-package #:nyxt-user)

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
 
(defmethod my-format-status-load-status ((status status-buffer))
  "Render the load status to HTML string"
  (let ((buffer (current-buffer (window status))))
    (if (web-buffer-p buffer)
        (case (slot-value buffer 'status)
          (:loading "∞ ")
          (:unloaded "∅ ")
          (:finished ""))
        "")))

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
            (:div :id "buffers"
                  (format nil "[~a/~a]"
                      buffer-count
                      (length (buffer-list))))
            ;;(:div :id "percentage"
            ;;      (format nil "L~a"
            ;;          (%percentage)))
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


