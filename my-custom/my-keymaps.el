;;; Key bindings
(global-set-key "\C-x\C-m"    'execute-extended-command)
(global-set-key "\C-c\C-m"    'execute-extended-command)
(global-set-key (kbd "C-,")   'scroll-down-line) ; Move 1 line down
(global-set-key (kbd "C-.")   'scroll-up-line)   ; Move 1 line up
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-M-]") '(lambda() (interactive) (beginning-of-defun -1)))

;; from Bozhidar Batsov Emacs Redux
(global-set-key (kbd "C-o")   'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)
(global-set-key (kbd "C-c i") 'find-user-init-file)

(provide 'my-keymaps)
