;; Time-stamp: <2017-12-02 22:57:46 csraghunandan>

;; All the macOS related configuration

(when (is-mac-p)
  (setq ns-function-modifier 'hyper)) ; make Fn key do Hyper

;; exec-path-from-shell: ensure environment variables inside Emacs look the same
;; as in the users shell
;; https://github.com/lunaryorn/osx-trash.el
(use-package exec-path-from-shell
  :init
  (when (is-mac-p)
    (exec-path-from-shell-initialize)))

;; delete files by moving to trash in macOS
;; https://github.com/lunaryorn/osx-trash.el
(use-package osx-trash
  :config
  (when (is-mac-p)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(provide 'setup-osx)
