;;; setup-desktop.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-13 23:07:02 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; desktop: save the current emacs session
(use-package desktop :defer 2
  :straight nil
  :config
  (setq desktop-save t
        desktop-load-locked-desktop nil)
  (desktop-save-mode 0)

  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
  ;; Save a bunch of variables to the desktop file.
  ;; For lists, specify the length of the maximal saved data too.
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (ivy-history              . 100)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)))

  ;; http://emacs.stackexchange.com/a/20036/115
  ;; fix warning upon restoring desktop save file
  (setq desktop-restore-frames nil)

  (defun rag/bury-star-buffers ()
    "Bury all star buffers."
    (mapc (lambda (buf)
            (when (string-match-p "\\`\\*.*\\*\\'" (buffer-name buf))
              (bury-buffer buf)))
          (buffer-list)))
  (add-hook 'desktop-after-read-hook #'rag/bury-star-buffers)

  (defun rag/restore-last-saved-desktop ()
    "Enable `desktop-save-mode' and restore the last saved desktop."
    (interactive)
    (desktop-save-mode 1)
    (desktop-read))

  :bind (("<S-f2>" . desktop-save-in-desktop-dir)
         ("<C-f2>" . rag/restore-last-saved-desktop)))

(provide 'setup-desktop)
