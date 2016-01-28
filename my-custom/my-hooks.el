(defun my-c-mode-hook ()
  "Setup C/C++ buffer indentation style and features."
  (c-set-style "stroustrup")
  (c-set-offset 'case-label '+) ; indent case labels by c-indent-level
  (set-fill-column 80)
  (auto-fill-mode t)
  (hs-minor-mode t)
  (yas-minor-mode t)
  (which-function-mode t)
  )

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)

(provide 'my-hooks)
