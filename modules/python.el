;;; python.el --- Python

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Virtual env, iPython

;;; Code:

(install-packages '(jedi
                    virtualenvwrapper
                    pytest))

(require 'virtualenvwrapper)
(require 'pytest)

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

(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs/")
(venv-workon "sc")

(setq eshell-prompt-function
      (lambda ()
        (concat "(" venv-current-name ") "
                (shrink-dir-names (replace-home (eshell/pwd)))
                " $ ")))

;; Venv mode line
(setq-default mode-line-format
              (cons '(:exec venv-current-name) mode-line-format))

;; IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 10000)

;; py.test
(setq pytest-global-name "py.test")
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c m") 'pytest-module)))

;; Flycheck - Python
(setq flycheck-flake8rc "~/.flake8.rc")

(eval-after-load 'python-mode
  '(progn
     (hc-highlight-trailing-whitespace t)
     (setq python-indent-offset 4)
     (setq python-indent 4)
     (subword-mode +1)))

(provide 'module-python)

;;; python.el ends here
