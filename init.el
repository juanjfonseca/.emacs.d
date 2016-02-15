;;; My custome packages
;; (add-to-list 'load-path "~/.emacs.d/my-custom")

;;; Load my packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-customizations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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

;; Setup Cscope
(require 'xcscope)
(cscope-setup)

;; Autoload modes for Git files.
(add-to-list 'load-path "~/.emacs.d/git-modes")

(autoload 'gitattributes-mode
  "gitattributes-mode.elc" "Enables the mode for .gitattributes, .git/info/attributes, and git/attributes files." t nil)
(dolist (pattern (list "/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'" "/git/attributes\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitattributes-mode)))

(autoload 'gitconfig-mode
  "gitconfig-mode.elc" "Enables the mode for .gitconfig, .git/config, git/config, and .gitmodules files." t nil)
(dolist (pattern (list "/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

(autoload 'gitignore-mode
  "gitignore-mode.elc" "Enables the mode for .gitignore, .git/info/exclude, and git/ignore files." t nil)
(dolist (pattern (list "/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;; (provide 'my-customizations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-defun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
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
  "Cleans the buffer"
  (interactive "*")
  (save-excursion
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))

;; (provide 'my-defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-gui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations
(blink-cursor-mode -1)
(column-number-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
(setq delete-by-moving-to-trash t) ; Move deleted files to Recycle.
(setq visible-bell t)
(size-indication-mode t)

;; (provide 'my-gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-keymaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings
(global-set-key "\C-x\C-m"    'execute-extended-command)
(global-set-key "\C-c\C-m"    'execute-extended-command)
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
(global-set-key (kbd "C-c i") 'find-user-init-file)

;; (provide 'my-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-modes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'generic-x)        ; Use Generic Modes For Obscure Languages.

;;; Define mode
(define-generic-mode
    'ttl-mode                ; name of the mode to create
  '(";")                     ; comments
  '(                         ; keywords
    ;;Communication commands
    "bplusrecv" "bplussend" "callmenu" "changedir" "clearscreen"
    "closett" "connect" "cygconnect" "disconnect" "dispstr" "enablekeyb"
    "flushrecv" "gethostname" "getmodemstatus" "gettitle" "kmtfinish"
    "kmtget" "kmtrecv" "kmtsend" "loadkeymap" "logautoclosemode"
    "logclose" "loginfo" "logopen" "logpause" "logrotate" "logstart"
    "logwrite" "quickvanrecv" "quickvansend" "recvln" "restoresetup"
    "scprecv" "scpsend" "send" "sendbreak" "sendbroadcast" "sendfile"
    "sendkcode" "sendln" "sendlnbroadcast" "sendmulticast" "setbaud"
    "setdebug" "setdtr" "setecho" "setmulticastname" "setrts" "setsync"
    "settitle" "showtt" "testlink" "unlink" "wait" "wait4all" "waitevent"
    "waitln" "waitn" "waitrecv" "waitregex" "xmodemrecv" "xmodemsend"
    "ymodemrecv" "ymodemsend" "zmodemrecv" "zmodemsend"
    ;;Control commands
    "break" "call" "continue" "do" "loop" "end" "execcmnd" "exit"
    "for" "next" "goto" "if" "then" "elseif" "else" "endif" "include"
    "mpause" "pause" "return" "until" "enduntil" "while" "endwhile"
    ;;String operation commands
    "code2str" "expandenv" "int2str" "regexoption" "sprintf"
    "sprintf2" "str2code" "str2int" "strcompare" "strconcat" "strcopy"
    "strinsert" "strjoin" "strlen" "strmatch" "strremove" "strreplace"
    "strscan" "strspecial" "strsplit" "strtrim" "tolower" "toupper"
    ;;File operation commands 
    "dirname" "fileclose" "fileconcat" "filecopy" "filecreate"
    "filedelete" "filelock" "filemarkptr" "fileopen" "filereadln"
    "fileread" "filerename" "filesearch" "fileseek" "fileseekback"
    "filestat" "filestrseek" "filestrseek2" "filetruncate" "fileunlock"
    "filewrite" "filewriteln" "findfirst" "findnext" "findclose"
    "foldercreate" "folderdelete" "foldersearch" "getdir" "getfileattr"
    "makepath" "setdir" "setfileattr" "Password" "commands" "delpassword"
    "getpassword" "ispassword" "passwordbox" "setpassword"
    ;;Miscellaneous commands
    "beep" "bringupbox" "checksum8" "checksum8file" "checksum16"
    "checksum16file" "checksum32" "checksum32file" "closesbox" "clipb2var"
    "crc16" "crc16file" "crc32" "crc32file" "exec" "dirnamebox"
    "filenamebox" "getdate" "getenv" "getipv4addr" "getipv6addr"
    "getspecialfolder" "gettime" "getttdir" "getver" "ifdefined"
    "inputbox" "intdim" "listbox" "messagebox" "random" "rotateleft"
    "rotateright" "setdate" "setdlgpos" "setenv" "setexitcode" "settime"
    "show" "statusbox" "strdim" "uptime" "var2clipb" "yesnobox"
    )
  '(("and" . 'font-lock-operator)    ; operators
    ("not" . 'font-lock-operator)
    ("or"  . 'font-lock-operator)
    ("xor" . 'font-lock-operator)
    (":"   . 'font-lock-operator))
  '("\\.ttl")                       ; files for which to activate this mode
  nil                                ; other functions to call
  "A mode for Teraterm macro files"  ; doc string for this mode
  )

;; (provide 'my-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-hooks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; (provide 'my-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options added by Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
