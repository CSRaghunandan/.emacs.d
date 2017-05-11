;; Time-stamp: <2017-05-12 01:21:16 csraghunandan>

;; configuration for buffers

;; prevent switching to a visible buffer
(setq switch-to-visible-buffer nil)

;; make buffers with same name unique
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; make emacs auto-refresh all buffers when files have changed on the disk
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)

;; get rid of all the tabs in a buffer
(defun rag/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)) nil)

;; transpose-frame: Transpose windows arrangement in a frame
;; https://www.emacswiki.org/emacs/transpose-frame.el
(use-package transpose-frame
  :bind (("C-x t f" . transpose-frame)
         ("C-x t r" . rotate-frame)))

;;; File Permissions
(defun modi/set-file-permissions (perm)
  "Change permissions of the file in current buffer.
Example: M-644 M-x modi/set-file-permissions."
  (interactive "p")
  (when (<= perm 1)
    (setq perm 644))
  (let ((cmd (concat "chmod "
                     (format "%s " perm)
                     (buffer-file-name))))
    (message "%s" cmd)
    (shell-command cmd "*Shell Temp*")
    (kill-buffer "*Shell Temp*")))

;;; *Messages* Auto-tail
;; Improved upon http://stackoverflow.com/a/4685005/1219634
(defun modi/messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name "*Messages*")
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))
(advice-add 'message :after #'modi/messages-auto-tail)

(defun rag/copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "Copy Buffer Name (F) Full, (D) Directory, (N) Name")
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

;;; Kill/Bury Buffer

;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2e4f4c9d48c563ff8bec102b66da0225587786c6
(>=e "26.0"
    nil  ;The `kill-current-buffer' command will be defined in core in emacs 26+
  (defun kill-current-buffer ()
    "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.
This is like `kill-this-buffer', but it doesn't have to be invoked
via the menu bar, and pays no attention to the menu-bar's frame."
    (interactive)
    (let ((frame (selected-frame)))
      (if (and (frame-live-p frame)
               (not (window-minibuffer-p (frame-selected-window frame))))
          (kill-buffer (current-buffer))
        (abort-recursive-edit)))))

(defun modi/kill-buffer-dwim (kill-next-error-buffer)
  "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.
If KILL-NEXT-ERROR-BUFFER is non-nil, kill the `next-error' buffer.
Examples of such buffers: *gtags-global*, *ag*, *Occur*."
  (interactive "P")
  (if kill-next-error-buffer
      (kill-buffer (next-error-find-buffer))
    (kill-current-buffer)))

(>=e "26.0"
  (bind-key "C-x k" 'modi/kill-buffer-dwim))

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

(bind-keys
 ("C-c o k" . rag/reopen-killed-file)
 ("C-c o K" . rag/reopen-killed-file-fancy)
 ("C-c r m" . delete-file-and-buffer)
 ("C-c m v" . rename-file-and-buffer)
 ("C-c m d" . make-directory)
 ("s-u" . revert-buffer-no-confirm)
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

(bind-keys
 ("C-x 2" . rag/split-below-and-move)
 ("C-x 3" . rag/split-right-and-move))

;;; Scratch-and-Back
;; http://emacs.stackexchange.com/a/81/115
(defun modi/switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.
        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'
Even if the current major mode is a read-only mode (derived from `special-mode'
or `dired-mode'), we would want to be able to write in the scratch buffer. So
the scratch major mode is set to `org-mode' for such cases.
Return the scratch buffer opened."
  (interactive "p")
  (if (and (or (null arg)               ; no prefix
               (= arg 1))
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0  "fundamental-mode") ; C-0
                       (4  "org-mode") ; C-u
                       (16 "emacs-lisp-mode") ; C-u C-u
                       ;; If the major mode turns out to be a `special-mode'
                       ;; derived mode, a read-only mode like `help-mode', open
                       ;; an `org-mode' scratch buffer instead.
                       (t (if (or (derived-mode-p 'special-mode) ; no prefix
                                  (derived-mode-p 'dired-mode))
                              "org-mode"
                            (format "%s" major-mode)))))
           (buf (get-buffer-create (concat "*scratch-" mode-str "*"))))
      (switch-to-buffer buf)
      (funcall (intern mode-str))   ; http://stackoverflow.com/a/7539787/1219634
      buf)))
(bind-key "C-c s b" 'modi/switch-to-scratch-and-back)

;;; Revert buffer
(defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; (message "buf:%s  filename:%s  modified:%s  filereadable:%s"
      ;;          buf filename
      ;;          (buffer-modified-p buf) (file-readable-p (format "%s" filename)))

      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defvar my-skippable-buffers '("*Messages*"
                               "*scratch*"
                               "*Help*"
                               "*Bookmark List*"
                               "*Ibuffer*"
                               "*compilation*")
  "Buffer names ignored by `my-next-buffer' and `my-previous-buffer'.")

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my-skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun my-next-buffer ()
  "Variant of `next-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'next-buffer))

(defun my-previous-buffer ()
  "Variant of `previous-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'previous-buffer))

(global-set-key [remap next-buffer] 'my-next-buffer)
(global-set-key [remap previous-buffer] 'my-previous-buffer)

;; recursive narrow or widen buffers
;; https://github.com/nflath/recursive-narrow/blob/master/recursive-narrow.el
;; press `C-x n w' when inside a recursive narrow to widen the buffer back to the
;; narrowed buffer instead of widening to the whole buffer
(use-package recursive-narrow)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(defun wh/switch-buffers-same-mode ()
  "Allows us to switch between buffers of the same major mode"
  (interactive)
  (let* ((matching-bufs (--filter (eq major-mode (with-current-buffer it major-mode))
                                  (buffer-list)))
         (bufs-with-names (--map
                           (cons
                            (let ((proj-name (with-current-buffer it (projectile-project-name))))
                              (if proj-name
                                  (format "%s (%s)" (buffer-name it) proj-name)
                                (buffer-name it)))
                            it)
                           matching-bufs))
         (chosen-buf
          (cdr (assoc (completing-read "Buffer: " bufs-with-names)
                      bufs-with-names))))
    (switch-to-buffer chosen-buf)))
(bind-key "C-x B" #'wh/switch-buffers-same-mode)

(defmacro my-special-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))

(defmacro my-special-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap end-of-buffer] ',fname))))))

(my-special-beginning-of-buffer dired
                                (while (not (ignore-errors (dired-get-filename)))
                                  (dired-next-line 1)))
(my-special-end-of-buffer dired
                          (dired-previous-line 1))

(my-special-beginning-of-buffer occur
                                (occur-next 1))
(my-special-end-of-buffer occur
                          (occur-prev 1))

(my-special-beginning-of-buffer ibuffer
                                (ibuffer-forward-line 1))
(my-special-end-of-buffer ibuffer
                          (ibuffer-backward-line 1))

(my-special-beginning-of-buffer org-agenda
                                (org-agenda-next-item 1))
(my-special-end-of-buffer org-agenda
                          (org-agenda-previous-item 1))

(provide 'setup-buffers)
