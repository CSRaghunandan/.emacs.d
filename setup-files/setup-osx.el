;; Time-stamp: <2017-09-04 15:30:26 csraghunandan>

;; All the macOS related configuration

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)) ; make Fn key do Hyper

;; exec-path-from-shell: ensure environment variables inside Emacs look the same
;; as in the users shell
;; https://github.com/lunaryorn/osx-trash.el
(use-package exec-path-from-shell
  :init
  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize)))

;; delete files by moving to trash in macOS
;; https://github.com/lunaryorn/osx-trash.el
(use-package osx-trash
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(provide 'setup-osx)
