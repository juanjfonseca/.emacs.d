;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'generic-x)        ; Use Generic Modes For Obscure Languages.

;;; Start the server once. 
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Introduced by emacs
(put 'narrow-to-region 'disabled nil)

;; Save privious location in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(delete-selection-mode t)      ; Delete Selection on Insert
(setq inhibit-splash-screen t) ; hide welcome screen

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Ispell location. The file must be in $PATH.
(setq ispell-program-name "aspell")

(provide 'my-customizations)
