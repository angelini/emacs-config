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
                    helm-projectile
                    helm-ag))

(require 'helm-config)
(require 'helm-projectile)

(helm-mode 1)

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-display-header-line nil)

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

;; Imenu keybinding
(global-set-key (kbd "C-x TAB")  'imenu)

;; Window move
(windmove-default-keybindings 'meta)

;; Support for Putty (Windows)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Multiple windows
(global-set-key (kbd "M-n") 'make-frame)
(global-set-key (kbd "M-`") 'other-frame)

;; Ace jump
(define-key global-map (kbd "C-;") 'ace-jump-mode)

;; Ag
(defun projectile-helm-ag ()
  "Search using ag in the current projectile root."
  (interactive)
  (helm-ag (projectile-project-root)))

(global-set-key (kbd "M-s") 'projectile-helm-ag)

(provide 'm-navigation)

;;; m-navigation.el ends here
