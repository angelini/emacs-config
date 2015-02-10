;;; rust.el --- Rust

;; Copyright (c) 2014 Alex Angelini
;;
;; Author: Alex Angelini <alex.louis.angelini@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Rust settings

;;; Code:

(install-packages '(rust-mode
		    flycheck-rust))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'm-rust)

;;; m-rust.el ends here
