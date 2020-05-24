;;; setup-buffers.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-24 20:11:17 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; configuration for buffers

;; prevent switching to a visible buffer
(setq switch-to-visible-buffer nil)

;;; Uniquify
;; The library uniquify overrides Emacs’ default mechanism for making buffer
;; names unique (using suffixes like <2>, <3> etc.) with a more sensible
;; behaviour which use parts of the file names to make the buffer names
;; distinguishable.
(use-package uniquify :straight nil
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; make emacs auto-refresh all buffers when files have changed on the disk
(global-auto-revert-mode t)
(setq auto-revert-remote-files t)
(setq auto-revert-verbose nil)

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

;; Display the file path of the file in current buffer and also copy it to
;; the kill-ring
;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun modi/copy-buffer-file-name (option &optional quiet)
  "Show the full path to the current file in the minibuffer and also copy it.
If the full file path has a sub-string \"xyz/\" where xyz is the
user name, replace that with \"${USER}/\".
If OPTION is \\='(4), copy only the file name (not the full path).
If OPTION is \\='(16), copy the full path without the environment
variable replacement.
If QUIET is non-nil, do not print the \"Copied file name ..\" message.
Return the copied file name."
  (interactive "P")
  (let* ((file-name-full (buffer-file-name))
         (file-name (when file-name-full
                      (cl-case (car option)
                        (4 (file-name-nondirectory file-name-full)) ;C-u
                        (16 file-name-full)                         ;C-u C-u
                        (t ;If $USER==xyz, replace xyz/ with ${USER}/ in file name
                         (replace-regexp-in-string ;No prefix
                          (concat user-login-name "/") "${USER}/" file-name-full))))))
    (if file-name
        (progn
          (kill-new file-name)
          (unless quiet
            (message "Copied file name `%s'" file-name))
          file-name)                    ;Return value
      (error "Buffer not visiting a file")
      nil)))

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

;;; Toggle between buffers
;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun toggle-between-buffers ()
  "Toggle between 2 buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))
(bind-chord "ZZ" #'toggle-between-buffers)

(defun modi/quit-and-kill-window ()
  "Quit window and kill instead of burying the buffer in it."
  (interactive)
  (quit-window :kill))

;;;; Read-only Buffer Bindings
;; Update bindings in few read-only modes
;; http://stackoverflow.com/a/27091776/1219634
;; Cannot set below to `'(map1 map2)'; it has to be `(list map1 map2)'.
(defconst modi/read-only-mode-maps (list special-mode-map
                                         tabulated-list-mode-map)
  "List of read-only mode maps in which few key bindings need to be updated.")
(dolist (map modi/read-only-mode-maps)
  (define-key map (kbd "y") #'bury-buffer)                ;Only bury
  (define-key map (kbd "k") #'modi/kill-buffer-dwim)      ;Only kill
  (define-key map (kbd "z") #'quit-window)                ;Quit + bury
  (define-key map (kbd "q") #'modi/quit-and-kill-window))  ;Quit + kill

;;; Current File Buffer Actions
;; Delete current buffer file
(defun modi/delete-current-buffer-file ()
  "Deletes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (file-exists-p filename)
               (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (message "File `%s' successfully deleted." filename))
    (kill-buffer (current-buffer))))

;; Rename current buffer file
;; http://www.whattheemacsd.com/
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer `%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named `%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File `%s' successfully renamed to `%s'."
                   name (file-name-nondirectory new-name)))))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(bind-keys
 ("C-c o k" . rag/reopen-killed-file)
 ("C-c o K" . rag/reopen-killed-file-fancy)
 ("C-c r m" . modi/delete-current-buffer-file)
 ("C-c m v" . rename-current-buffer-file)
 ("C-c m d" . make-directory)
 ("H-u" . revert-buffer-no-confirm)
 ("C-c s n" . modi/copy-buffer-file-name))

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
  (if (and (or (null arg)               ;No prefix
               (= arg 1))
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0 "fundamental-mode") ;C-0
                       (4 "org-mode")         ;C-u
                       (16 "emacs-lisp-mode") ;C-u C-u
                       ;; If the major mode turns out to be a `special-mode'
                       ;; derived mode, a read-only mode like `help-mode', open
                       ;; an `org-mode' scratch buffer instead.
                       (t (if (or (derived-mode-p 'special-mode) ;No prefix
                                  (derived-mode-p 'dired-mode))
                              "org-mode"
                            (format "%s" major-mode)))))
           (buf (get-buffer-create (concat "*scratch-" mode-str "*"))))
      (switch-to-buffer buf)
      (funcall (intern mode-str))    ;http://stackoverflow.com/a/7539787/1219634
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
          (let (kill-buffer-query-functions) ;No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

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

;; beginend: Emacs package to redefine M-< and M-> for some modes
;; https://github.com/DamienCassou/beginend
(use-package beginend
  :hook (ivy-occur-grep-mode . beginend-ivy-occur-mode)
  :init (beginend-global-mode)
  :config
  (beginend-define-mode ivy-occur-mode
    (progn
      (ivy-occur-next-line 4))
    (progn
      (ivy-occur-previous-line 1))))

(defun duplicate-buffer (new-name)
  "Create a copy of the current buffer with the filename NEW-NAME.
The original buffer and file are untouched."
  (interactive (list (read-from-minibuffer "New name: " (buffer-file-name))))

  (let ((filename (buffer-file-name))
        (new-directory (file-name-directory new-name))
        (contents (buffer-substring (point-min) (point-max))))
    (unless filename (error "Buffer '%s' is not visiting a file!" (buffer-name)))

    (make-directory new-directory t)
    (find-file new-name)
    (insert contents)
    (basic-save-buffer)))

;;; Narrow/Widen
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun endless/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.
With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond
          ((ignore-errors (org-edit-src-code) t))
          ((ignore-errors (org-narrow-to-block) t))
          (t
           (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t
         (narrow-to-defun))))
(bind-key "C-x n n" #'endless/narrow-or-widen-dwim)

;; Don't kill the important buffers
(defconst mu-do-not-kill-buffer-names '("*scratch*"
                                        "*Messages*"
                                        "*Bookmark List*")
  "Names of buffers that should not be killed.")

;;;###autoload
(defun mu-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) mu-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

;; Don't kill important buffers
(add-hook 'kill-buffer-query-functions #'mu-do-not-kill-important-buffers)

;; vlf: View large files without slowing emacs to a crawl
;; https://github.com/m00natic/vlfi
(use-package vlf :defer t
  :config
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

(provide 'setup-buffers)
