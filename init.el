;;; package --- Summary
;;; Commentary:
;;; Code:
;; Setup the GUI as early as posible to keep the display from flashing and changing at start up.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(column-number-mode t)
(global-display-line-numbers-mode t)
(size-indication-mode t)
(setq delete-by-moving-to-trash t) ; Move deleted files to Recycle.
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
;;;;;;;;;;;;;;;;;;;
;; end GUI setup ;;
;;;;;;;;;;;;;;;;;;;

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/custom")

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package anzu
  :ensure t)

(use-package async
  :ensure t)

(use-package avy
  :ensure t)

(use-package clean-aindent-mode
  :ensure t
  :config
  (require 'clean-aindent-mode)
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c c") 'clang-format-buffer))

(use-package comment-dwim-2
  :ensure t)

(use-package company
  :ensure t
  :config
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (delete 'company-semantic company-backends))

(use-package counsel
  :ensure t)

(use-package counsel-gtags
  :ensure t
  :config
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode))

(use-package counsel-projectile
  :ensure t)

(use-package duplicate-thing
  :ensure t)

(use-package epl
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package ggtags
  :ensure t)

(use-package iedit
  :ensure t)

(use-package ivy
   :ensure t)

 (use-package magit
  :ensure t)

(use-package pkg-info
  :ensure t)

(use-package popup
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (require 'projectile)
  (projectile-mode)
  (setq projectile-enable-caching t))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1))

(use-package swiper
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package volatile-highlights
  :ensure t)

(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package zzz-to-char
  :ensure t)

(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-my-modes)
(require 'setup-my-keybinds)
(require 'setup-my-defuns)

(windmove-default-keybindings)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; C style “stroustrup”: What Stroustrup, the author of C++ used in his book
(setq c-default-style "stroustrup")

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(ede-project-directories (quote ("/devspace/apx/apx_core/apx_core_project")))
 '(fci-rule-color "#073642")
 '(global-display-line-numbers-mode t)
 '(ivy-mode t)
 '(minimap-mode t)
 '(minimap-window-location (quote right))
 '(package-selected-packages
   (quote
    (counsel-gtags counsel-projectile counsel ivy flycheck auto-highlight-symbol clang-format spacemacs-theme zzz-to-char zygospore yasnippet ws-butler volatile-highlights use-package undo-tree magit iedit ggtags duplicate-thing company comment-dwim-2 clean-aindent-mode anzu)))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))
(provide 'init)
;;; init.el ends here
