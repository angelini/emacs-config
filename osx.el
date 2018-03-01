;;; osx.el --- OSX related configuration -*- lexical-binding: t; -*-

(global-set-key (kbd "M-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "M-w") 'clipboard-kill-region)
(global-set-key (kbd "M-v") 'clipboard-yank)

(when (display-graphic-p)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))
