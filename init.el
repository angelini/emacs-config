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

;; Terminal
(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; Packages
(use-package company
  :init (global-company-mode)
  :bind (("TAB" . #'company-indent-or-complete-common))
  :config (setq company-tooltip-align-annotations t
                company-idle-delay 2))

(use-package dumb-jump
  :init (dumb-jump-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package flycheck
  :demand t
  :hook  ((after-init . global-flycheck-mode)
          (flycheck-before-syntax-check . projectile-pyenv-mode-set))
  :config (progn
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                                       c/c++-gcc
                                                       html-tidy))
            (setq flycheck-check-syntax-automatically '(save mode-enabled)
                  flycheck-pylintrc "pylintrc"
                  flycheck-clang-language-standard "c++11")))

(use-package focus-autosave-mode
  :config (focus-autosave-mode))

(use-package helm
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("M-s" . helm-ag-project-root))
  :config (setq helm-split-window-inside-p t
                helm-display-header-line nil
                helm-buffers-fuzzy-matching t
                helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s"))

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package projectile
  :init (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :init (helm-projectile-on)
  :config (setq projectile-completion-system 'helm
                projectile-switch-project-action 'helm-projectile))

(use-package web-mode)

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("C-y" . yas-expand))
  :config (yas-global-mode 1))

;; Language Packages
(use-package cider
  :config (setq cider-show-error-buffer nil
                nrepl-hide-special-buffers t))

(use-package clojure-mode
  :config (add-hook 'clojure-mode-hook #'smartparens-mode))

(use-package company-irony
  :config (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :init (setq gofmt-command "goimports")
  :hook (before-save . gofmt-before-save)
  :bind ("M-." . godef-jump))

(use-package irony
  :hook ((c++-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package pyenv-mode
  :init (setq elpy-rpc-ignored-buffer-size 1024000)
  :config (pyenv-mode))

(use-package elpy
  :config (elpy-enable)
  :bind (:map elpy-mode-map
              ("M-<up>" . nil)
              ("M-<down>" . nil)
              ("M-<left>" . nil)
              ("M-<right>" . nil))
  :hook (elpy-mode . (lambda () (highlight-indentation-mode -1))))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package racket-mode)

(use-package rust-mode
  :hook (rust-mode . smartparens-mode))

(use-package sql-indent
  :init (eval-after-load "sql"
          '(load-library "sql-indent")))

(use-package typescript-mode)

(use-package utop
  :init (setq utop-command "opam config exec -- utop -emacs"))

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

;; Console
(unless (display-graphic-p)
  (load-local "console"))
