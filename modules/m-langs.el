;;; langs.el --- Programming language settings

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Modes and customizations for various programming languages

;;; Code:

(install-packages '(scala-mode2
                    yaml-mode))

;;; Javascript

(install-package 'coffee-mode)

(require 'coffee-mode)
(defvar js-indent-level)

(setq coffee-tab-width 2)
(setq js-indent-level 2)

(eval-after-load 'coffee-mode
  '(progn
     (subword-mode +1)))

;;; Python

(install-packages '(jedi
                    virtualenvwrapper
                    pytest))

(require 'python)

(defun replace-home (pwd)
  "Replace home dir in PWD with ~."
  (replace-regexp-in-string "^/Users/alexangelini" "~" pwd))

(defun shrink-dir-names (pwd)
  "Shrink all dir names except the last one in PWD."
  ((lambda (d-list)
     (concat
      (mapconcat (lambda (d) (if (string= "" d) "" (substring d 0 1)))
                 (butlast d-list)
                 "/")
      (when (> (length d-list) 1) "/")
      (car (last d-list))))
   (split-string pwd "/")))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

(custom-set-variables '(eshell-prompt-function
                        (quote (lambda ()
                                 (concat "(" venv-current-name ") "
                                         (shrink-dir-names (replace-home (eshell/pwd)))
                                         " $ ")))))

;; Venv mode line
(setq-default mode-line-format
              (cons '(:exec venv-current-name) mode-line-format))

;; Enable venv
(when (file-exists-p "~/.virtualenvs")
  (setq venv-location "~/.virtualenvs/")
  (venv-workon "sc"))

;; Jedi
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 10000)

;; py.test
(require 'pytest)
(setq pytest-global-name "py.test")
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c m") 'pytest-module)))

;; Flycheck - Python
(require 'flycheck)
(setq flycheck-flake8rc "~/.flake8rc")

;;; Rust

(install-packages '(rust-mode
                    flycheck-rust))

(eval-after-load 'python-mode
  '(progn
     (setq python-indent-offset 4)
     (subword-mode +1)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;;; Ruby

(install-package 'inf-ruby)

;; Add other file types as ruby files
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

(eval-after-load 'ruby-mode
  '(progn
     (subword-mode +1)))

;;; Go

(install-package 'go-mode)

(add-to-list 'exec-path "~/packages/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)

(setenv "GOROOT" "/Users/alexangelini/packages/go")

;;; Shell

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))

(provide 'm-langs)

;;; m-langs.el ends here
