;; Time-stamp: <2018-06-22 12:17:28 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; All the macOS related configuration

(when (is-mac-p)
  (setq ns-function-modifier 'hyper ; make Fn key do Hyper
        ns-use-thin-smoothing t
        ns-use-mwheel-momentum t
        ns-use-mwheel-acceleration t
        pixel-scroll-mode t))

;; exec-path-from-shell: ensure environment variables inside Emacs look the same
;; as in the users shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (is-mac-p)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; delete files by moving to trash in macOS
;; https://github.com/lunaryorn/osx-trash.el
(use-package osx-trash
  :if (is-mac-p)
  :config
  (setq delete-by-moving-to-trash t)
  (osx-trash-setup))

(provide 'setup-osx)
