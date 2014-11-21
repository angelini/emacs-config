;;; osx.el --- OSX specific customizations

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Specific tweaks only for OSX

;;; Code:

(defun set-path-from-shell-PATH ()
  "Copy the value of zsh's PATH into the envionrment."
  (let ((path-from-shell (shell-command-to-string "TERM=xterm-256color $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-path-from-shell-PATH))

;; Meta and alt keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(provide 'm-osx)

;;; osx.el ends here
