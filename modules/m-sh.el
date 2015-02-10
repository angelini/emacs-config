;;; sh.el --- Shell

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Shell mode settings

;;; Code:

(require 'term)

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "M-v") 'term-paste)))

(provide 'm-sh)

;;; m-sh.el ends here
