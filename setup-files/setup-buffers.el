;; Time-stamp: <2016-12-17 12:05:32 csraghunandan>

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

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(bind-keys*
 ("C-c o k" . rag/reopen-killed-file)
 ("C-c o K" . rag/reopen-killed-file-fancy)
 ("C-c r m" . delete-file-and-buffer)
 ("C-x k" . rag/kill-a-buffer)
 ("C-c m v" . rename-file-and-buffer)
 ("C-c m d" . make-directory)
 ("<f5>" . revert-buffer-no-confirm)
 ("C-c s n" . rag/copy-buffer-file-name-as-kill))

;; diminish auto-revert-mode emacs
(use-package autorevert
  :diminish auto-revert-mode)

(defun rag/split-below-and-move ()
  "split window below and move there"
  (interactive)
  (split-window-below)
  (other-window 1))
(defun rag/split-right-and-move ()
  "split window right and move there"
  (interactive)
  (split-window-right)
  (other-window 1))

(bind-keys*
 ("C-x 2" . rag/split-below-and-move)
 ("C-x 3" . rag/split-right-and-move))

(provide 'setup-buffers)
