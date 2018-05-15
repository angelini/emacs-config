;; init.el -*- lexical-binding: t; -*-

(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'f)
(require 'use-package)

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "defuns")

(set-path-from-shell-PATH)
(setq default-directory (f-full (getenv "HOME")))

;; Default mode
(setq-default major-mode 'text-mode)
(set-language-environment "UTF-8")

;; Default Thresholds
(setq gc-cons-threshold 50000000) ;; 50MB
(setq large-file-warning-threshold 100000000) ;; 100MB

;; Theme
(load-theme 'zenburn t)
(set-face-attribute 'default nil :height 160 :family "Source Code Pro")

(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(set-default 'truncate-lines t)

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      scroll-step 1)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Navigation
(windmove-default-keybindings 'meta)
(global-set-key (kbd "M-DEL") 'backward-delete-word)

;; Autosave buffers
(setq make-backup-files nil)
(setq auto-save-default t)
(setq auto-save-visited-file-name t)

;; Indentation settings
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Parens
(require 'smartparens-config)
(show-paren-mode 1)

;; Toggle quotes
(global-set-key (kbd "C-'") 'toggle-quotes)

;; Scratch
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Copy & paste
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)
(global-set-key (kbd "C-S-v") 'clipboard-yank)

;; Symlinks
(setq vc-follow-symlinks t)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Packages
(use-package company
  :init (global-company-mode)
  :bind (("TAB" . #'company-indent-or-complete-common))
  :config (setq company-tooltip-align-annotations t
                company-idle-delay 0))

(use-package dumb-jump
  :init (dumb-jump-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package flycheck
  :demand t
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled)
          flycheck-pylintrc "pylintrc")
    (setq-default flycheck-disabled-checkers '(python-flake8 emacs-lisp-checkdoc c/c++-gcc))
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package focus-autosave-mode
  :init (focus-autosave-mode))

(use-package helm
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("M-s" . helm-ag-project-root))
  :config (setq helm-split-window-in-side-p t
                helm-buffers-fuzzy-matching t
                helm-display-header-line nil
                helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s"))

(use-package helm-projectile
  :init (helm-projectile-on)
  :config (setq projectile-completion-system 'helm
                projectile-switch-project-action 'helm-projectile))

(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package projectile
  :init (projectile-global-mode))

(use-package web-mode)

(use-package yasnippet
  :init (yas-global-mode 1)
  :config
  (progn
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-y") 'yas-expand)))

;; Language Packages
(use-package cider
  :config (setq cider-show-error-buffer nil
                nrepl-hide-special-buffers t))

(use-package clojure-mode
  :config (add-hook 'clojure-mode-hook #'smartparens-mode))

(use-package company-irony
  :init (add-to-list 'company-backends 'company-irony))

(use-package company-jedi
  :init (add-to-list 'company-backends 'company-jedi))

(use-package flycheck-irony
  :config (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package irony
  :config (progn
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package pyenv-mode
  :init (pyenv-mode)
  :config (add-hook 'flycheck-before-syntax-check-hook #'projectile-pyenv-mode-set))

(use-package racer
  :config
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'race-mode-hook #'eldoc-mode)))

(use-package racket-mode)

(use-package rust-mode)

;; C Mode
(c-add-style "custom-c"
             '("custom"
               (c-basic-offset . 4)
               (c-offsets-alist
                (innamespace . -))))
(setq c-default-style "custom-c")

;; Python Mode
(add-hook 'python-mode #'smartparens-mode)

;; Ruby Mode
(add-hook 'ruby-mode #'smartparens-mode)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

;; Shell Mode
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2
                                sh-indentation 2)))

;; MacOS overrides
(when (eq system-type 'darwin)
  (load-local "osx"))
