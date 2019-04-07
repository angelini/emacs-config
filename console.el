;;; console.e --- Console related configurations -*- lexical-binding: t; -*-

(global-set-key (kbd "M-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "M-x") 'clipboard-kill-region)
(global-set-key (kbd "M-v") 'clipboard-yank)

(require 'xt-mouse)
(defun xterm-mouse--tracking-sequence (suffix)
  "Return a control sequence to enable or disable mouse tracking.
SUFFIX is the last character of each escape sequence (?h to
enable, ?l to disable).

Custom implementation to only support basic mouse motions"
  (mapcar
   (lambda (code) (format "\e[?%d%c" code suffix))
   `(1000 1002)))

(xterm-mouse-mode 1)
