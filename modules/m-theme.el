;;; theme.el --- Look and feel

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Font, color scheme and other visual tweaks

;;; Code:

(install-package 'color-theme-solarized)

(load-theme 'solarized-light t)

(set-face-attribute 'default nil :height 120 :family "Source Code Pro")

(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(tool-bar-mode -1)

(provide 'm-theme)

;;; theme.el ends here
