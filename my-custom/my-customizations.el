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

;; Ispell location
(setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")

(provide 'my-customizations)
