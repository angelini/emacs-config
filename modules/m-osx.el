;;; osx.el --- OSX specific customizations

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Specific tweaks only for OSX

;;; Code:

;; Meta and alt keys
(when (display-graphic-p)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(provide 'm-osx)

;;; m-osx.el ends here
