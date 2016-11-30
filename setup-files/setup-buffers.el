;; Time-stamp: <2016-11-18 15:49:27 csraghunandan>

;; configuration for buffers

;; prevent switching to a visible buffer
(setq switch-to-visible-buffer nil)

;; make buffers with same name unique
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; resize windows proportionally
(setq window-combination-resize t)

;; make emacs auto-refresh all buffers when files have changed on the disk
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)

;; get rid of all the tabs in a buffer
(defun rag/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  nil)

(defun rag/copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defun rag/make-backup ()
  "Make a backup copy of current file.
The backup file name has the form ‹name›~‹timestamp›~, in the same dir.
If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done."
  (interactive)
  (if (buffer-file-name)
      (let* ((currentName (buffer-file-name))
             (backupName (concat currentName
                                 "." (format-time-string "%Y%m%d_%H%M") ".bkp")))
        (copy-file currentName backupName :overwrite-if-already-exists)
        (message (concat "Backup saved as: " (file-name-nondirectory backupName))))
    (user-error "buffer is not a file.")))

;;; Reopen Killed File
;; http://emacs.stackexchange.com/a/3334/115
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun rag/reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (if killed-file-list
      (find-file (pop killed-file-list))
    (message "No recently killed file found to reopen.")))

(defun rag/reopen-killed-file-fancy ()
  "Pick a file to revisit from a list of files killed during this
Emacs session."
  (interactive)
  (if killed-file-list
      (let ((file (completing-read "Reopen killed file: " killed-file-list
                                   nil nil nil nil (car killed-file-list))))
        (when file
          (setq killed-file-list (cl-delete file killed-file-list :test #'equal))
          (find-file file)))
    (error "No recently-killed files to reopen")))

(defun rag/kill-a-buffer (askp)
  "with no prefix, kill the current buffer without prompt
with prefix, select which buffer to kill"
  (interactive "P")
  (if askp
      (kill-buffer (funcall completing-read-function
                            "Kill buffer: "
                            (mapcar #'buffer-name (buffer-list))))
    (kill-this-buffer)))

(defun rag/delete-file-visited-by-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "b")
  (let* ((buffer (get-buffer buffername))
         (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (kill-buffer-ask buffer))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(bind-keys*
 ("C-c o k" . rag/reopen-killed-file)
 ("C-c d f" . rag/delete-file-visited-by-buffer)
 ("C-x k" . rag/kill-a-buffer)
 ("C-c n n" . rename-file)
 ("C-c m d" . make-directory)
 ("<f6>" . rag/make-backup)
 ("<f5>" . revert-buffer-no-confirm)
 ("C-c b n" . rag/copy-buffer-file-name-as-kill))

;; diminish auto-revert-mode emacs
(use-package autorevert
  :diminish auto-revert-mode)

(provide 'setup-buffers)
