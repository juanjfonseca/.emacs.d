;;; Customizations
(blink-cursor-mode -1)
(column-number-mode t)
(global-linum-mode t)
(setq delete-by-moving-to-trash t) ; Move deleted files to Recycle.
(setq visible-bell t)
(size-indication-mode t)
(load-theme 'solarized-dark 1)

;; Show current function on top line.
(setq-default header-line-format
              '((which-function-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      (assq-delete-all 'which-function-mode mode-line-misc-info))
(which-function-mode t)

(provide 'my-gui)
