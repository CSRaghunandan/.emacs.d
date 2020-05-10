;;; setup-packages.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-10 09:10:31 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; enable imenu support for `use-package'
(setq use-package-enable-imenu-support t)

;; this makes each use-package form also invoke straight.el to install the
;; package, unless otherwise specified.
(setq straight-use-package-by-default t)

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

;; (unless (package-installed-p 'use-package) ; unless it is already installed
;;   (package-refresh-contents) ; update packages archive
;;   (package-install 'use-package)) ; install the latest version of use-package
;; (eval-when-compile (require 'use-package))
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
