;;; setup-packages.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-24 17:49:31 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; enable imenu support for `use-package'
(setq use-package-enable-imenu-support t)

(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; update packages archive
  (package-install 'use-package)) ; install the latest version of use-package
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; https://github.com/emacsorphanage/key-chord/tree/master
(use-package key-chord
  :hook (emacs-startup . (lambda ()
                           (key-chord-mode 1))))

(use-package use-package-chords)

;; updates GPG keys used by ELPA package manager
;; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
(use-package gnu-elpa-keyring-update)

;; http://emacs.stackexchange.com/a/26513/115
(defun modi/package-dependency-check-ignore (orig-ret)
  "Remove the `black listed packages' from ORIG-RET.
Packages listed in the let-bound `pkg-black-list' will not be auto-installed
even if they are found as dependencies.
It is known that this advice is not effective when installed packages
asynchronously using `paradox'. Below is effective on synchronous
package installations."
  (let ((pkg-black-list '(org))
        new-ret
        pkg-name)
    ;; (message "before %S" orig-ret)
    (dolist (pkg-struct orig-ret)
      (setq pkg-name (package-desc-name pkg-struct))
      (if (member pkg-name pkg-black-list)
          (message (concat "Package `%s' will not be installed. "
                           "See `modi/package-dependency-check-ignore'.")
                   pkg-name)
        ;; (message "Package to be installed: %s" pkg-name)
        (push pkg-struct new-ret)))
    ;; Tue Apr 11 17:48:16 EDT 2017 - kmodi
    ;; It's *very* critical that the order of packages stays the same in NEW-RET
    ;; as in ORIG-RET. The `push' command flips the order, so use `reverse'
    ;; to flip the order back to the original.
    ;;   Without this step, you will get errors like below when installing
    ;; packages with dependencies:
    ;;   Debugger entered--Lisp error: (error "Unable to activate package ‘nim-mode’.
    ;;   Required package ‘flycheck-28’ is unavailable")
    (setq new-ret (reverse new-ret))
    ;; (message "after  %S" new-ret)
    new-ret))
(advice-add 'package-compute-transaction :filter-return #'modi/package-dependency-check-ignore)

(provide 'setup-packages)
