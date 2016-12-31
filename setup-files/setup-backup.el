;; Time-stamp: <2016-12-31 10:21:27 csraghunandan>

;; Backup settings for emacs.

;; disable auto safe
(setq auto-save-default nil)

(defun save-all ()
  ;; save all modified buffers without asking
  (interactive)
  (save-some-buffers t))

;; save all modified buffers when focus-out
(add-hook 'focus-out-hook 'save-all)

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
