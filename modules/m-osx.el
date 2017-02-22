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
  (let ((path (shell-command-to-string "source $HOME/.profile && printf $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string path ":"))))

(when window-system (set-path-from-shell-PATH))

;; Meta and alt keys
(when (display-graphic-p)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(provide 'm-osx)

;;; m-osx.el ends here
