;; Time-stamp: <2017-02-16 11:19:41 csraghunandan>

;; dired: file system manager for emacs
(use-package dired :ensure nil
  :bind (:map dired-mode-map
              ("S" . ora-dired-get-size))
  :config
  (progn
    ;; mark symlinks
    (setq dired-ls-F-marks-symlinks t)
    ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-copies 'always)
    ;; Never prompt for recursive deletes of a directory
    (setq dired-recursive-deletes 'always)
    ;; makes dired guess the target directory
    (setq dired-dwim-target t)

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -G : Do not print group names like 'users'
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables,'/' to directories, etc.
    ;; default value for dired: "-al"
    (setq dired-listing-switches "-alGhvF --group-directories-first")

    ;; auto-revert dired buffers if file changed on disk
    (setq dired-auto-revert-buffer t)

    (defun rag/dired-rename-buffer-name ()
      "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
      (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "*Dired* " name "/") t))))

    (add-hook 'dired-mode-hook #'rag/dired-rename-buffer-name))

  ;; dired-quick-sort: hydra to sort files in dired
  ;; Press `S' to invoke dired-quick-sort hydra
  ;; https://gitlab.com/xuhdev/dired-quick-sort
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
                ("s" . hydra-dired-quick-sort/body)))

  (defvar du-program-name (executable-find "du"))
  (defun ora-dired-get-size ()
    "Get the size of a folder recursively"
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process du-program-name nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1))))))

  ;; dired-x: to hide uninteresting files in dired
  (use-package dired-x :ensure nil
    :config
    (setq dired-omit-verbose nil)
    ;; hide backup, autosave, *.*~ files
    ;; omit mode can be toggled using `C-x M-o' in dired buffer.
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$"))))

;; dired+: extensions for `dired-mode'
;; https://www.emacswiki.org/emacs/DiredPlus
(use-package dired+
  :config
  (require 'dired+)
  ;; reuse dired directories instead of opening a thousand `dired' buffers
  (diredp-toggle-find-file-reuse-dir 1)

  ;; show more details by default
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil)

  ;; rewise multiple open files so that it only opens one window
  (define-key dired-mode-map (kbd "F")
    (lambda ()
      (interactive)
      (mapc #'find-file (reverse (dired-get-marked-files))))))

(require 'dired-aux)

(defalias 'dired-do-create-files 'lawlist-dired-do-create-files)

(defun lawlist-dired-do-create-files (op-symbol file-creator operation arg
                                                &optional marker-char op1 how-to)
  "(1) If the path entered by the user in the mini-buffer ends in a trailing
forward slash /, then the code assumes the path is a directory -- to be
created if it does not already exist.; (2) if the trailing forward slash
is omitted, the code prompts the user to specify whether that path is a
directory."
  (or op1 (setq op1 operation))
  (let* (
         skip-overwrite-confirmation
         (fn-list (dired-get-marked-files nil arg))
         (rfn-list (mapcar (function dired-make-relative) fn-list))
         (dired-one-file  ; fluid variable inside dired-create-files
          (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
         (target-dir
          (if dired-one-file
              (dired-get-file-for-visit) ;; filename if one file
            (dired-dwim-target-directory))) ;; directory of multiple files
         (default (and dired-one-file
                       (expand-file-name (file-name-nondirectory (car fn-list))
                                         target-dir)) )
         (defaults (dired-dwim-target-defaults fn-list target-dir))
         (target (expand-file-name ; fluid variable inside dired-create-files
                  (minibuffer-with-setup-hook (lambda ()
                                                (set (make-local-variable 'minibuffer-default-add-function) nil)
                                                (setq minibuffer-default defaults))
                    (dired-mark-read-file-name
                     (concat (if dired-one-file op1 operation) " %s to: ")
                     target-dir op-symbol arg rfn-list default))))
         (unmodified-initial-target target)
         (into-dir (cond ((null how-to)
                          (if (and (memq system-type '(ms-dos windows-nt cygwin))
                                   (eq op-symbol 'move)
                                   dired-one-file
                                   (string= (downcase
                                             (expand-file-name (car fn-list)))
                                            (downcase
                                             (expand-file-name target)))
                                   (not (string=
                                         (file-name-nondirectory (car fn-list))
                                         (file-name-nondirectory target))))
                              nil
                            (file-directory-p target)))
                         ((eq how-to t) nil)
                         (t (funcall how-to target)))))
    (if (and (consp into-dir) (functionp (car into-dir)))
        (apply (car into-dir) operation rfn-list fn-list target (cdr into-dir))
      (or into-dir (setq target (directory-file-name target)))
      ;; create new directories if they do not exist.
      (when
          (and
           (not (file-directory-p (file-name-directory target)))
           (file-exists-p (directory-file-name (file-name-directory target))))
        (let ((debug-on-quit nil))
          (signal 'quit `(
                          "A file with the same name as the proposed directory already
exists."))))
      (when
          (and
           (not (file-exists-p (directory-file-name (expand-file-name
                                                     target))))
           (or
            (and
             (null dired-one-file)
             (not (string-match "\\(.*\\)\\(/$\\)"
                                unmodified-initial-target)))
            (not (file-directory-p (file-name-directory target)))
            (string-match "\\(.*\\)\\(/$\\)" unmodified-initial-target)) )
        (let* (
               new
               list-of-directories
               list-of-shortened-directories
               string-of-directories-a
               string-of-directories-b
               (max-mini-window-height 3)
               (expanded (directory-file-name (expand-file-name target)))
               (try expanded) )
          ;; Find the topmost nonexistent parent dir (variable `new')
          (while (and try (not (file-exists-p try)) (not (equal new try)))
            (push try list-of-directories)
            (setq new try
                  try (directory-file-name (file-name-directory try))))
          (setq list-of-shortened-directories
                (mapcar
                 (lambda (x) (concat "..." (car (cdr (split-string x try)))))
                 list-of-directories))
          (setq string-of-directories-a
                (combine-and-quote-strings list-of-shortened-directories))
          (setq string-of-directories-b (combine-and-quote-strings
                                         (delete (car (last list-of-shortened-directories))
                                                 list-of-shortened-directories)))
          (if
              (and
               (not (string-match "\\(.*\\)\\(/$\\)"
                                  unmodified-initial-target))
               ;; (cdr list-of-directories)
               dired-one-file
               (file-exists-p dired-one-file)
               (not (file-directory-p dired-one-file)))
              (if (y-or-n-p
                   (format "Is `%s` a directory?" (car (last
                                                        list-of-directories))))
                  (progn
                    (or (y-or-n-p (format "@ `%s`, create:  %s" try
                                          string-of-directories-a))
                        (let ((debug-on-quit nil))
                          (signal 'quit `("You have exited the function."))))
                    (make-directory expanded t)
                    (setq into-dir t))
                (if (equal (file-name-directory target) (file-name-directory
                                                         dired-one-file))
                    (setq new nil)
                  (or (y-or-n-p
                       (format "@ `%s`, create:  %s" try
                               string-of-directories-b))
                      (let ((debug-on-quit nil))
                        (signal 'quit `("You have exited the function."))))
                  (make-directory (car (split-string
                                        (car (last list-of-directories))
                                        (concat "/" (file-name-nondirectory target)))) t)
                  (setq target (file-name-directory target))
                  (setq into-dir t) ))
            (or (y-or-n-p (format "@ `%s`, create:  %s" try
                                  string-of-directories-a))
                (let ((debug-on-quit nil))
                  (signal 'quit `("You have exited the function."))))
            (make-directory expanded t)
            (setq into-dir t) )
          (when new
            (dired-add-file new)
            (dired-move-to-filename))
          (setq skip-overwrite-confirmation t) ))
      (lawlist-dired-create-files file-creator operation fn-list
                                  (if into-dir      ; target is a directory
                                      (function (lambda (from)
                                                  (expand-file-name (file-name-nondirectory from) target)))
                                    (function (lambda (_from) target)))
                                  marker-char skip-overwrite-confirmation ))))

(defun lawlist-dired-create-files (file-creator operation fn-list
                                                name-constructor
                                                &optional marker-char skip-overwrite-confirmation)
  (let (dired-create-files-failures failures
                                    skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query overwrite-backup-query)
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite (not skip-overwrite-confirmation)
                       (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
              ;; If DESTNAME is a subdirectory of FROM, not a symlink,
              ;; and the method in use is copying, signal an error.
              (and (eq t (car (file-attributes destname)))
                   (eq file-creator 'dired-copy-file)
                   (file-in-directory-p destname from)
                   (error "Cannot copy `%s' into its subdirectory `%s'"
                          from to)))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d" operation success-count total)
                  (dired-add-file to actual-marker-char))
              (file-error    ; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format "%s failed for %d file%s in %d requests"
               operation (length failures)
               (dired-plural-s (length failures))
               total)
       failures))
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
               operation (length failures)
               total (dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
               operation (length skipped) total
               (dired-plural-s total))
       skipped))
     (t
      (message "%s: %s file%s"
               operation success-count (dired-plural-s success-count)))))
  (dired-move-to-filename))

(provide 'setup-dired)
