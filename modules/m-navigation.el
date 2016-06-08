;;; navigation.el --- Project navigation

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Helm, projectile

;;; Code:

(install-packages '(avy
                    projectile
                    helm
                    helm-projectile
                    helm-ag
                    dumb-jump))

(require 'helm-config)
(require 'helm-projectile)

(helm-mode 1)

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-display-header-line nil)

(global-set-key (kbd "M-x") 'helm-M-x)

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

;; Avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-line)

;; Ag
(global-set-key (kbd "M-s") 'helm-do-ag-project-root)
(global-set-key (kbd "M-S") 'helm-do-ag-this-file)

;; Dumb Jump
(dumb-jump-mode)

;; Delete word
(defun delete-word (arg)
  "Delete characters forward from ARG until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward from ARG until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-DEL") 'backward-delete-word)

;; iSearch recenter
(defadvice
  isearch-repeat-forward
  (after isearch-repeat-forward-recenter activate)
  (recenter))

(defadvice
  isearch-repeat-backward
  (after isearch-repeat-backward-recenter activate)
  (recenter))

(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)

(provide 'm-navigation)

;;; m-navigation.el ends here
