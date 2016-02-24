;;; langs.el --- Programming language settings

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Modes and customizations for various programming languages

;;; Code:

(install-packages '(company
                    scala-mode2
                    yaml-mode
                    markdown-mode
                    toml))

;;; Javascript

(install-packages '(coffee-mode
                    js2-mode))

(require 'coffee-mode)
(require 'js2-mode)
(defvar js-indent-level)

(setq coffee-tab-width 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-strict-trailing-comma-warning nil)

(eval-after-load 'coffee-mode
  '(progn
     (subword-mode +1)))

(add-hook 'js-mode-hook 'js2-minor-mode)

;;; Clojure

(install-packages '(clojure-mode
                    cider
                    kibit-mode
                    paredit))

(require 'cider)
(setq cider-show-error-buffer nil)
(setq nrepl-hide-special-buffers t)

(add-hook 'clojure-mode-hook #'company-mode)
(add-hook 'clojure-mode-hook #'flycheck-mode)

;; Eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Cider refresh
(defun cider-namespace-refresh ()
  "Refreshes current namespace in the Cider REPL."
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(require 'clojure-mode)
(define-key clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

;;; Python

(install-packages '(jedi
                    virtualenvwrapper
                    pytest
                    ob-ipython))

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

(eval-after-load 'python-mode
  '(progn
     (setq python-indent-offset 4)
     (subword-mode +1)))

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
            (local-set-key (kbd "C-c C-m") 'pytest-module)))

;; Flycheck
(require 'flycheck)
(setq flycheck-flake8rc "~/.flake8rc")
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(ruby ruby-rubocop ruby-jruby)))

;;; Rust

(install-packages '(rust-mode
                    flycheck-rust
                    racer
                    rustfmt))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;; Racer
(require 'racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(setq racer-cmd "~/src/racer/target/release/racer")
(setq racer-rust-src-path "~/src/rustc-1.6-beta1/src/")

(define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)

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

;; Flycheck
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(ruby ruby-rubocop ruby-jruby)))

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

;;; CSS

(require 'css-mode)
(setq css-indent-offset 2)

;;; Markdown

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (setq word-wrap t)))

;;; C

(setq c-default-style "linux"
      c-basic-offset 4)

(provide 'm-langs)
;;; m-langs.el ends here
