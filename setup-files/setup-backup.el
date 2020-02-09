;;; setup-backup.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-10 00:07:14 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; Backup settings for emacs.

;; No need for ~ files when editing
(setq create-lockfiles nil)

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 7   ;; Number of newest versions to keep.
      kept-old-versions 2   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Also backup files which are version controlled
(setq vc-make-backup-files t)

;; Also make emacs ignore that appended string to the backup files when
;; deciding the major mode
;; http://emacs.stackexchange.com/a/13285/115
(add-to-list 'auto-mode-alist '("\\.[0-9_]+\\.bkp\\'" nil backup-file))

(provide 'setup-backup)
