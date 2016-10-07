;; Time-stamp: <2016-10-07 11:05:34 csraghunandan>

;; dired
;; file system manager for emacs
(use-package dired :ensure nil
  :config
  (progn
    (setq delete-by-moving-to-trash t)
    ;; mark symlinks
    (setq dired-ls-F-marks-symlinks t)
    ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-copies 'always)
    ;; Never prompt for recursive deletes of a directory
    (setq dired-recursive-deletes 'always)
    ;; fix `ls' for macOS.
    (setq insert-directory-program "gls" dired-use-ls-dired t)
    (setq dired-dwim-target t)
    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -G : Do not print group names like 'users'
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables,
    ;;       '/' to directories, etc.
    ;; default value for dired: "-al"
    (setq dired-listing-switches "-alGhvF --group-directories-first")
    (defun rag/dired-rename-buffer-name ()
      "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
      (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "*Dired* " name "/") t))))

    (add-hook 'dired-mode-hook #'rag/dired-rename-buffer-name))

  ;; dired-x - to hide uninteresting files in dired
  (use-package dired-x :ensure nil
    :config
    (progn
      (setq dired-omit-verbose nil)
      ;; hide backup, autosave, *.*~ files
      ;; omit mode can be toggled using `C-x M-o' in dired buffer.
      (add-hook 'dired-mode-hook #'dired-omit-mode)
      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$")))))

;; dired-narrow
;; https://github.com/Fuco1/dired-hacks
(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(provide 'setup-dired)
