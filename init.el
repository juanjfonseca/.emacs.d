;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My custome packages ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Enable auto-complete on every mode
(ac-config-default)

;; Setup Cscope
(require 'xcscope)
(cscope-setup)

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

(defun ac ()
  "Change directory to apx_core"
  (interactive)
  (cd "/devspace/apx/apx_core")
  (cscope-setup)
  (cscope-minor-mode))

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

(require 'powerline)
(powerline-default-theme)

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

;; Magit
(global-set-key (kbd "C-c m") 'magit-status)

;; clang-format
(global-set-key (kbd "C-c c") 'clang-format-region)

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

(font-lock-add-keywords 'c-mode
                        '(("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))

(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(CHECK\\|CHECK_TEXT\\|CHECK_EQUAL\\|CHECK_THROWS\\)\\>" . font-lock-function-name-face)
                          ("\\<\\(STRCMP_EQUAL\\|LONGS_EQUAL\\|BYTES_EQUAL\\)\\>" . font-lock-function-name-face)
                          ("\\<\\(POINTERS_EQUAL\\|DOUBLES_EQUAL\\|FAIL\\)\\>" . font-lock-function-name-face)))

;; (provide 'my-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options added by Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(clang-format-executable "clang-format-3.8")
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cscope-close-window-after-select t)
 '(cscope-truncate-lines nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "8f0334c430540bf45dbcbc06184a2e8cb01145f0ae1027ce6b1c40876144c0c9" "0788bfa0a0d0471984de6d367bb2358c49b25e393344d2a531e779b6cec260c5" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "86a731bda96ed5ed69980b4cbafe45614ec3c288da3b773e4585101e7ece40d2" "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
