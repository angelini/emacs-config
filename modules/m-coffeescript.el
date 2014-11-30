;;; coffeescript.el --- Coffeescript

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Coffeescript settings

;;; Code:

(install-package 'coffee-mode)

(custom-set-variables '(coffee-tab-width 2))

(eval-after-load 'coffee-mode
  '(progn
     (subword-mode +1)))

(provide 'm-coffeescript)

;;; m-coffeescript.el ends here
