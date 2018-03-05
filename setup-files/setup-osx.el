;; Time-stamp: <2018-03-05 14:33:53 csraghunandan>

;; All the macOS related configuration

(when (is-mac-p)
  (setq ns-function-modifier 'hyper)) ; make Fn key do Hyper

;; exec-path-from-shell: ensure environment variables inside Emacs look the same
;; as in the users shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (is-mac-p)
  :init (exec-path-from-shell-initialize))

;; delete files by moving to trash in macOS
;; https://github.com/lunaryorn/osx-trash.el
(use-package osx-trash
  :if (is-mac-p)
  :config
  (setq delete-by-moving-to-trash t)
  (osx-trash-setup))

(provide 'setup-osx)
