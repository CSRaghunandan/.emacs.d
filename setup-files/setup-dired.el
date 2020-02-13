;;; setup-dired.el -*- lexical-binding: t -*-
;; Time-stamp: <2020-02-13 17:47:13 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; dired: file system manager for emacs
(use-package dired :ensure nil
  :bind ((:map dired-mode-map
               ("S" . ora-dired-get-size)
               ("E" . ora-ediff-files)
               ("^" . rag/dired-up-dir)
               ("C-a" . dired-back-to-start-of-files)))
  :hook ((dired-mode . (lambda ()
                         (setq-local tab-width 1))))
  :config
  (progn
    ;; follow symlinks in dired
    (setq find-file-visit-truename t)

    ;; C-a is nicer in dired if it moves back to start of files
    (defun dired-back-to-start-of-files ()
      (interactive)
      (backward-char (- (current-column) 2)))

    ;; mark symlinks
    (setq dired-ls-F-marks-symlinks t)
    ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-copies 'always)
    ;; Never prompt for recursive deletes of a directory
    (setq dired-recursive-deletes 'always)
    ;; makes dired guess the target directory
    (setq dired-dwim-target t)

    (>=e "27.0"
        (setq dired-create-destination-dirs t))

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables,'/' to directories, etc.
    ;; default value for dired: "-al"
    (setq dired-listing-switches (if (is-windows-p)
                                     "-alh"
                                   "-alhvF --group-directories-first"))

    ;; auto-revert dired buffers if file changed on disk
    (setq dired-auto-revert-buffer t)

    (defun rag/dired-rename-buffer-name ()
      "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
      (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "*Dired* " name "/") t))))

    (add-hook 'dired-mode-hook #'rag/dired-rename-buffer-name))

  ;;* rest
  (defun ora-dired-get-size ()
    (interactive)
    (let* ((cmd (concat "du -sch "
                        (mapconcat (lambda (x) (shell-quote-argument (file-name-nondirectory x)))
                                   (dired-get-marked-files) " ")))
           (res (shell-command-to-string cmd)))
      (if (string-match "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$" res)
          (message (match-string 1 res))
        (error "unexpected output %s" res))))

  ;; set some programs to run externally
  (setq dired-guess-shell-alist-user
        '(("\\.jpg\\'" "feh")
          ("\\.png\\'" "feh")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|ifo\\|m4v\\|wmv\\|webm\\)\\(?:\\.part\\)?\\'"
           "vlc")
          ("\\.html?\\'" "firefox-developer-edition")
          ("\\.pdf\\'" "zathura")
          ("\\.\\(?:pptx?\\|od[ts]\\|xls[xbm]?\\|docx?\\)\\'" "libreoffice")
          ("\\.csv\\'" "libreoffice")))

  (eval-after-load "recentf"
    '(progn
       (defun recentf-track-opened-file ()
         "Insert the name of the dired or file just opened or written into the recent list."
         (let ((buff-name (or buffer-file-name (and (derived-mode-p 'dired-mode) default-directory))))
           (and buff-name
                (recentf-add-file buff-name)))
         ;; Must return nil because it is run from `write-file-functions'.
         nil)

       (defun recentf-track-closed-file ()
         "Update the recent list when a file or dired buffer is killed.
  That is, remove a non kept file from the recent list."
         (let ((buff-name (or buffer-file-name (and (derived-mode-p 'dired-mode) default-directory))))
           (and buff-name
                (recentf-remove-if-non-kept buff-name))))
       (add-hook 'dired-after-readin-hook 'recentf-track-opened-file)))

  ;; use the same buffer for going up a directory in dired
  (defun rag/dired-up-dir()
    (interactive) (find-alternate-file ".."))

  ;; https://oremacs.com/2017/03/18/dired-ediff/
  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name "file: "
                                         (dired-dwim-target-directory)))))
            (when (file-newer-than-file-p file1 file2)
              (cl-rotatef file1 file2))
            (if (string-match "current ar archive" (shell-command-to-string (format "file %s" file1)))
                (async-shell-command
                 (format "hexdump-diffuse %s %s"
                         (shell-quote-argument file1)
                         (shell-quote-argument file2)))
              (ediff-files file1 file2)
              (add-hook 'ediff-after-quit-hook-internal
                        (lambda ()
                          (setq ediff-after-quit-hook-internal nil)
                          (set-window-configuration wnd)))))
        (error "no more than 2 files should be marked")))

    (setq dired-garbage-files-regexp
          "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'"))

  ;; dired-x: to hide uninteresting files in dired
  (use-package dired-x :ensure nil
    :demand t
    :hook ((dired-mode . dired-omit-mode))
    :config
    (setq dired-omit-verbose nil)
    ;; hide backup, autosave, *.*~ files
    ;; omit mode can be toggled using `C-x M-o' in dired buffer.
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$"))))

;; diredfl:Extra Emacs font lock rules for a more colourful dired
;; https://github.com/purcell/diredfl/tree/085eabf2e70590ec8e31c1e66931d652d8eab432
(use-package diredfl
  :config (diredfl-global-mode))

(use-package wdired :defer t
  :ensure nil
  :config (setq wdired-allow-to-change-permissions t))

;; dired-quick-sort: hydra to sort files in dired
;; Press `S' to invoke dired-quick-sort hydra
;; https://gitlab.com/xuhdev/dired-quick-sort
(use-package dired-quick-sort
  :bind (:map dired-mode-map
              ("s" . hydra-dired-quick-sort/body)))

(provide 'setup-dired)
