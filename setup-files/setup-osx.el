;; Time-stamp: <2017-01-03 20:27:06 csraghunandan>

;; All the macOS related configuration

(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; to make sure emacs gets the $PATH variable from shell
;; emacs shell environment is different from the shell in mac
;; https://github.com/lunaryorn/osx-trash.el
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'setup-osx)
