(in-package #:nyxt-user)

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


