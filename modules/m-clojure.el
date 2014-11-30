;;; clojure.el --- Clojure

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Clojure settings

;;; Code:

(install-packages '(clojure-mode
                    cider))

;; Eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-show-error-buffer nil)
(setq nrepl-hide-special-buffers t)

(provide 'm-clojure)

;;; m-clojure.el ends here
