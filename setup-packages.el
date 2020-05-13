;;; setup-packages.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-13 16:11:16 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; this makes each use-package form also invoke straight.el to install the
;; package, unless otherwise specified.
(setq straight-use-package-by-default t)
;; Use watchexec (if it is installed) and python3 to check for modifications for
;; elisp packages
(when (executable-find "watchexec")
  (setq straight-check-for-modifications
                             `(watch-files find-when-checking)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)
;; enable imenu support for `use-package'
(setq use-package-enable-imenu-support t)

(require 'bind-key)

;; https://github.com/emacsorphanage/key-chord/tree/master
(use-package key-chord
  :hook (emacs-startup . (lambda ()
                           (key-chord-mode 1))))

(use-package use-package-chords)

;; updates GPG keys used by ELPA package manager
;; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
(use-package gnu-elpa-keyring-update)

(provide 'setup-packages)
