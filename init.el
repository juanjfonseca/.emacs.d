;;; initfile --- Summary:
;;; Commentary:
;; Emacs 25.1 and newer tested
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup the GUI as early as posible to keep the   ;;
;; display from flashing and changing at start up. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      `((".*" . ,temporary-file-directory))
      )
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      )

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
  :ensure t
  :config
  (require 'anzu)
  (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  )

(use-package async
  :ensure t)

(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1))
  )

(use-package clean-aindent-mode
  :ensure t
  :config
  (require 'clean-aindent-mode)
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  )

(use-package clang-format
  :ensure t
  :bind (("C-c c" . clang-format-buffer))
  )

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2)
  )

(use-package company
  :ensure t
  :config
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-." . counsel-imenu)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git-grep)
         ("C-c j" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         )
  )

(use-package counsel-gtags
  :ensure t
  :config
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))
  )

(use-package counsel-projectile
  :ensure t)

(use-package duplicate-thing
  :ensure t
  :config
  (require 'duplicate-thing)
  :bind (("C-c d" . duplicate-thing))
  )

(use-package epl
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  )

(use-package ggtags
  :ensure t)

(use-package iedit
  :ensure t
  :config
  (require 'iedit)
  (setq iedit-toggle-key-default nil)
  :bind ("C-;" . iedit-mode)
  )

(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :config
  (require 'ivy)
  (ivy-mode t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  )

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status)
  )

(use-package pkg-info
  :ensure t)

(use-package popup
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (require 'projectile)
  (projectile-mode)
  (setq projectile-enable-caching t)
  )

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  )

(use-package undo-tree
  :ensure t
  :config
  (require 'undo-tree)
  (global-undo-tree-mode)
  )

(use-package volatile-highlights
  :ensure t
  :config
  (require 'volatile-highlights)
  (volatile-highlights-mode t)
  )

(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all)
  )

;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet)
  )

(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-to-char)
         ("M-Z" . zzz-up-to-char))
  )

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-m")    'execute-extended-command)
(global-set-key (kbd "C-c C-m")    'execute-extended-command)
(global-set-key (kbd "C-,")   'scroll-down-line) ; Move 1 line down
(global-set-key (kbd "C-.")   'scroll-up-line)   ; Move 1 line up
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-M-]") '(lambda() (interactive) (beginning-of-defun -1)))
(global-set-key (kbd "C-o")   'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
(require 'generic-x)        ; Use Generic Modes For Obscure Languages.

;; Autoload modes for Git files.
(add-to-list 'load-path "~/.emacs.d/git-modes")

(autoload 'gitattributes-mode
  "gitattributes-mode.el" "Enables the mode for .gitattributes, .git/info/attributes, and git/attributes files." t nil)
(dolist (pattern (list "/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'" "/git/attributes\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitattributes-mode)))

(autoload 'gitconfig-mode
  "gitconfig-mode.el" "Enables the mode for .gitconfig, .git/config, git/config, and .gitmodules files." t nil)
(dolist (pattern (list "/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

(autoload 'gitignore-mode
  "gitignore-mode.el" "Enables the mode for .gitignore, .git/info/exclude, and git/ignore files." t nil)
(dolist (pattern (list "/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;; Reload files that change on disk.
(auto-revert-mode)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename) (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window.
From Bozhidar Batsov Emacs Redux"
  (interactive)
  (find-file user-init-file))

(defun cleanup-buffer ()
  "Cleans the current buffer."
  (interactive "*")
  (save-excursion
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Editing -> Editing Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-default whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate-functions t)))

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single \
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")


(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region (BEG and END) isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, \
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes', \
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup)))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; move point from window to window using meta (Alt) and the arrow keys.
(windmove-default-keybindings)
(windmove-default-keybindings 'meta)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; C style “stroustrup”: What Stroustrup, the author of C++ used in his book
(setq-default c-default-style "stroustrup")

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; setup GDB
(setq-default
 ;; use gdb-many-windows by default
 gdb-many-windows t)

;;;;;;;;;;;;;;;
;; ECB Setup ;;
;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/ecb")
(load-file "~/.emacs.d/ecb/ecb.el")
(require 'ecb-autoloads)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables ;;
;;;;;;;;;;;;;;;;;;;;;;
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
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.50")
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
 '(default ((t (:family "Courier 10 Pitch" :foundry "bitstream" :slant normal :weight normal :height 113 :width normal)))))
(provide 'init)
;;; init.el ends here
