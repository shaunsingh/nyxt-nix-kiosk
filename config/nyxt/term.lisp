(in-package #:nyxt-user)

;;; provide interaction with xterm.js

(define-mode xterm-mode (nyxt/mode/passthrough:pasthrough-mode)
  "Launch tor & set proxy to local Tor SOCKS5 proxy."
  ((uiop:launch-program "sudo wetty")))

(define-command-global open-terminal ()
  "Open a terminal in a new buffer"
  (let ((term-buffer (make-buffer :title "*xterm*"
                                  :url "http://localhost:3000/wetty"
                                  :modes 'xterm-mode)))
    (set-current-buffer term-buffer)))

(define-configuration xterm-mode
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              `(body
                :background-color ,*base00-*)
              `("#terminal"
                :padding "9px 27px 18px 27px")))))
