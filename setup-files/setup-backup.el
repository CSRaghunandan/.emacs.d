;; Time-stamp: <2018-06-22 12:11:12 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; Backup settings for emacs.

;; disable auto safe
(setq auto-save-default nil)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 7   ;; Number of newest versions to keep.
      kept-old-versions 2   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Also backup files which are version controlled
(setq vc-make-backup-files t)

(setq rag/backup-directory
      (let ((dir (concat temporary-file-directory
                         (getenv "USER") "/.backups/"))) ; must end with /
        (make-directory dir :parents)
        dir))

;; Save all backups to `rag/backup-directory'
(setq backup-directory-alist `(("." . ,rag/backup-directory)))
(message (format "All backup files will be saved to %s." rag/backup-directory))

;; http://ergoemacs.org/emacs/elisp_make-backup.html
(defun modi/make-backup ()
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
;; Also make emacs ignore that appended string to the backup files when
;; deciding the major mode
;; http://emacs.stackexchange.com/a/13285/115
(add-to-list 'auto-mode-alist '("\\.[0-9_]+\\.bkp\\'" nil backup-file))

(provide 'setup-backup)
