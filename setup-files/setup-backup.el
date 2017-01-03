;; Time-stamp: <2017-01-03 17:53:46 csraghunandan>

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

;; Default and per-save backups go here:
(setq backup-directory-alist `(("" . ,(concat user-emacs-directory "backup/per-save/"))))

(provide 'setup-backup)
