;; Time-stamp: <2016-10-17 21:00:38 csraghunandan>

;; All the macOS related configuration

(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; open the current active buffer in finder
;; https://github.com/kaz-yos/reveal-in-osx-finder
(use-package reveal-in-osx-finder
  :bind ("s-z" . reveal-in-osx-finder))

;; delete files by moving to trash
;; https://github.com/lunaryorn/osx-trash.el
(use-package osx-trash
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

;; to make sure emacs gets the $PATH variable from shell
;; emacs shell environment is different from the shell in mac
;; https://github.com/lunaryorn/osx-trash.el
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'setup-osx)
