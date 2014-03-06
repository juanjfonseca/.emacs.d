(defun my-c-mode-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'case-label '+) ; indent case labels by c-indent-level
  (set-fill-column 80)
  (auto-fill-mode t)
  (hs-minor-mode t)
  (which-function-mode t)
  (turn-on-ctags-auto-update-mode)
  (projectile-mode t))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(provide 'my-hooks)
