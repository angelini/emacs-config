;;; defuns.el --- Functions -*- lexical-binding: t; -*-

(defun delete-word (arg)
  "Delete characters forward from ARG until encountering the end of a word.
With argument, do this that many times."
  (interactive "P")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward from ARG until encountering the end of a word.
With argument, do this that many times."
  (interactive "P")
  (delete-word (- arg)))

(defun set-path-from-shell-PATH ()
  "Copy the value of bash's PATH into the envionrment."
  (let ((path (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string path ":"))))

(defun helm-ag-project-root (arg)
  "Use AG to search the projectile project root for ARG."
  (interactive "P")
  (helm-grep-ag (projectile-project-root) arg))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (when (and (member project (pyenv-mode-versions))
               (not (equal project (pyenv-mode-version))))
      (pyenv-mode-set project)
      (pyvenv-activate project))))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
