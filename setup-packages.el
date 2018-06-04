;; Time-stamp: <2018-06-04 14:44:32 csraghunandan>

(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; install the latest version of use-package
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; add imenu support for use-package declarations
(setq use-package-enable-imenu-support t)

;; Manage your installed packages with emacs
;; https://github.com/jabranham/system-packages
(use-package system-packages)

;; https://github.com/emacsorphanage/key-chord/tree/master
(use-package key-chord
  :config (key-chord-mode 1))

(use-package use-package-chords)
(use-package use-package-ensure-system-package)

;; Mark packages to *not* to be updated
;; http://emacs.stackexchange.com/a/9342/115
(defvar modi/package-menu-dont-update-packages '(org)
  "List of packages for which the package manager should not look for updates.
Example: '(org org-plus-contrib).")
;; Do not upgrade Org using the package manager if it's set to *not* use the
;; Elpa version of Org.
(add-to-list 'modi/package-menu-dont-update-packages 'org-plus-contrib)

(defun modi/package-menu-remove-excluded-packages (orig-fun &rest args)
  "Remove the packages listed in `modi/package-menu-dont-update-packages' from
the `tabulated-list-entries' variable."
  (let ((included (-filter
                   (lambda (entry)
                     (let ((pkg-name (package-desc-name (car entry))))
                       (not (member pkg-name modi/package-menu-dont-update-packages))))
                   tabulated-list-entries)))
    (setq-local tabulated-list-entries included)
    (apply orig-fun args)))
(advice-add 'package-menu--find-upgrades :around #'modi/package-menu-remove-excluded-packages)

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
