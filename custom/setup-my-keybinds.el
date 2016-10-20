;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup-my-keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings
(global-set-key (kbd "C-,")   'scroll-down-line) ; Move 1 line down
(global-set-key (kbd "C-.")   'scroll-up-line)   ; Move 1 line up
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-M-]") '(lambda() (interactive) (beginning-of-defun -1)))

;; Remap zap-to-char to zzz-to-char
(global-set-key "\M-z" 'zzz-to-char)
(global-set-key "\M-Z" 'zzz-up-to-char)

;; from Bozhidar Batsov Emacs Redux
(global-set-key (kbd "C-o")   'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)

;; Magit
(global-set-key (kbd "C-c m") 'magit-status)

;; clang-format
(global-set-key (kbd "C-c c") 'clang-format-region)

(provide 'setup-my-keybinds)
