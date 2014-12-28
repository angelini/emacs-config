;;; javascript.el --- Javascript

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Javascript settings

;;; Code:

(install-package 'coffee-mode)

(require 'coffee-mode)
(defvar js-indent-level)

(setq coffee-tab-width 2)
(setq js-indent-level 2)

(eval-after-load 'coffee-mode
  '(progn
     (subword-mode +1)))

(provide 'm-javascript)

;;; m-javascript.el ends here
