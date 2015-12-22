;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'generic-x)        ; Use Generic Modes For Obscure Languages.

;; Save privious location in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(server-start)
(delete-selection-mode t)      ; Delete Selection on Insert
(setq inhibit-splash-screen t) ; hide welcome screen

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Ispell location. The file must be in $PATH.
(setq ispell-program-name "aspell")

;; Show current function name in all modes
(which-function-mode t)

(provide 'my-customizations)
