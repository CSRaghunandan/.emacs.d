;; Time-stamp: <2016-12-29 09:57:44 csraghunandan>

;; Backup settings for emacs.

;; auto-save all files every 3 minutes of idle time
(setq auto-save-default t
      auto-save-timeout 180 ;; save files after 3 minutes of idle time
      auto-save-interval 0) ;; don't save files based on number of keystrokes

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
