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

(defvar root-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar modules-dir (expand-file-name "modules" root-dir))

(add-to-list 'load-path modules-dir)

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar package-list
  '(org
    yasnippet
    magit
    multi-term
    flycheck
    isend-mode
    scratch
    toggle-quotes
    smartparens
    company))

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
(require 'm-langs)

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
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)
(global-set-key (kbd "C-S-v") 'clipboard-yank)

;; Splash screen
(setq inhibit-splash-screen t)

;; Path
(defun set-path-from-shell-PATH ()
  "Copy the value of bash's PATH into the envionrment."
  (let ((path (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string path ":"))))

(when window-system (set-path-from-shell-PATH))

;; Magit
(global-set-key (kbd "C-c m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

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

;; Scratch
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Flycheck
(setq flycheck-check-syntax-automatically '(save
                                            mode-enabled))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Org file
(define-key global-map (kbd "C-c t")
  (lambda()
    (interactive)
    (find-file "/Users/alexangelini/Dropbox/org/work.org")))

(define-key global-map (kbd "C-c y")
  (lambda()
    (interactive)
    (find-file "/Users/alexangelini/Dropbox/org/home.org")))

;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)))

(setq org-confirm-babel-evaluate nil)

;; Starscream
(define-key global-map (kbd "C-c s")
  (lambda()
    (interactive)
    (with-output-to-temp-buffer "*schedule*"
      (shell-command "/Users/alexangelini/src/starscream/bin/schedule_yaml" "*schedule*" "*Messages*")
      (pop-to-buffer "*schedule*")
      (yaml-mode))))


;; Snippets
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-y") 'yas-expand)

;; Shell
(require 'term)
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "M-v") 'term-paste)))

;; isend-mode
(add-hook 'isend-mode-hook 'isend-default-shell-setup)
(add-hook 'isend-mode-hook 'isend-default-ipython-setup)

;; Revert all buffers
(defun revert-buffer-keep-undo ()
  "Revert buffer but keep undo history."
  (interactive)
  (let ((inhibit-read-only t))
    (clear-visited-file-modtime)
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (buffer-modified-p)))
        (revert-buffer-keep-undo))))
  (message "Refreshed open files."))

(define-key global-map (kbd "C-c r") 'revert-all-buffers)

;; Close all other buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(define-key global-map (kbd "C-c k") 'kill-other-buffers)

;; Company mode
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0)

;; Toggle quotes
(global-set-key (kbd "C-'") 'toggle-quotes)

;; Smart Parens
(require 'smartparens-config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt-function
   (quote
    (lambda nil
      (concat "(" venv-current-name ") "
              (shrink-dir-names
               (replace-home
                (eshell/pwd)))
              " $ "))))
 '(package-selected-packages
   (quote
    (lua-mode dumb-jump zenburn-theme yasnippet yaml-mode virtualenvwrapper toml toggle-quotes smartparens scratch scala-mode rustfmt racket-mode racer pytest paredit ob-ipython multi-term markdown-mode magit js2-mode jedi isend-mode inf-ruby helm-projectile helm-ag go-mode flycheck-rust company color-theme-solarized coffee-mode cider avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
