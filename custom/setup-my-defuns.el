;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'setup-my-defuns)
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

(provide 'setup-my-defuns)
