;;; init.el --- Initialize and load modules

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Load modules

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

(require 'cl)
(require 'package)

(defvar root-dir (file-name-directory load-file-name))
(defvar modules-dir (expand-file-name  "modules" root-dir))

(add-to-list 'load-path modules-dir)

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar package-list
  '(ag
    magit
    multi-term
    flycheck
    scala-mode2
    yaml-mode))

(defvar require-refresh t)

(defun refresh-packages ()
  "Refresh packages at most once."
  (when require-refresh
    (package-refresh-contents)
    (setq require-refresh nil)))

(defun all-installed-p ()
  "Check that all packages are installed."
  (cl-every #'package-installed-p package-list))

(defun install-package (package)
  "Install PACKAGE unless it's already installed."
  (unless (memq package package-list)
    (add-to-list 'package-list package))
  (unless (package-installed-p package)
    (refresh-packages)
    (package-install package)))

(defun install-packages (packages)
  "Install PACKAGES unless they are already installed."
  (mapc 'install-package packages))

(unless (all-installed-p)
  (install-packages package-list))

(require 'm-navigation)
(require 'm-theme)
(require 'm-python)
(require 'm-clojure)
(require 'm-ruby)
(require 'm-javascript)
(require 'm-sh)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'm-osx))

;; Increase GC threshold to 50MB
(setq gc-cons-threshold 50000000)

;; Increase file size warning to 100MB
(setq large-file-warning-threshold 100000000)

;; Set UTF-8 as the default language env
(set-language-environment "UTF-8")

;; Copy & paste
(global-set-key (kbd "M-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "M-w") 'clipboard-kill-region)
(global-set-key (kbd "M-v") 'clipboard-yank)

;; Splash screen
(setq inhibit-splash-screen t)

;; Magit
(global-set-key (kbd "C-c m") 'magit-status)

;; Symlinks
(setq vc-follow-symlinks t)

;; Default mode
(setq-default major-mode 'text-mode)

;; Server
(server-start)

;; Buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Indentation settings
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Flycheck
(setq flycheck-check-syntax-automatically '(save
                                            mode-enabled))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Todo
(define-key global-map (kbd "C-c t")
  (lambda()
    (interactive)
    (find-file "/Users/alexangelini/Desktop/todo.txt")))

;; Revert all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed open files."))

(define-key global-map (kbd "C-c r") 'revert-all-buffers)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
