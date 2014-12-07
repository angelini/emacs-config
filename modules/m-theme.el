;;; theme.el --- Look and feel

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Font, color scheme and other visual tweaks

;;; Code:

(install-packages '(color-theme-solarized
                    zenburn-theme))

(load-theme 'zenburn t)

(set-face-attribute 'default nil :height 140 :family "Source Code Pro")

;; Bell
(setq ring-bell-function 'ignore)

;; Decorations
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Cursor
(blink-cursor-mode -1)

;; Line wrap
(set-default 'truncate-lines t)

;; Line wrapping
(set-default 'truncate-lines t)

;; Paren mode
(show-paren-mode t)

;; Lambda symbols
(global-prettify-symbols-mode 1)

(provide 'm-theme)

;;; m-theme.el ends here
