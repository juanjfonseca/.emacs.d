;;; package --- Summary
;;; Commentary:
;;; GROUP: MODE -> Mode definitions
;;; Code:
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
  '("\\.ttl")                        ; files for which to activate this mode
  nil                                ; other functions to call
  "A mode for Teraterm macro files"  ; doc string for this mode
  )

;; Autoload modes for Git files.
(add-to-list 'load-path "~/.emacs.d/custom/git-modes")

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

(provide 'setup-my-modes)
;;; setup-my-modes.el ends here
