;;; clojure.el --- Clojure

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Clojure settings

;;; Code:

(install-packages '(clojure-mode
                    cider
                    kibit-mode
                    company))

;; Eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-show-error-buffer nil)
(setq nrepl-hide-special-buffers t)

(add-hook 'clojure-mode-hook 'flycheck-mode)

;; Cider refresh
(defun cider-namespace-refresh ()
  "Refreshes current namespace in the Cider REPL."
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(require 'clojure-mode)
(define-key clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

;; Autocomplete
(global-company-mode)

(provide 'm-clojure)

;;; m-clojure.el ends here
