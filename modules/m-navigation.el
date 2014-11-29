;;; navigation.el --- Project navigation

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Helm, projectile

;;; Code:

(install-packages '(ace-jump-mode
                    projectile
                    helm
                    helm-projectile))

(require 'helm-config)
(require 'helm-projectile)

(helm-mode 1)

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t)

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

;; Window move
(windmove-default-keybindings 'meta)

;; Support for Putty (Windows)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(provide 'm-navigation)

;;; navigation.el ends here
