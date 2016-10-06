(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(use-package reveal-in-osx-finder
  :bind ("s-z" . reveal-in-osx-finder))

;; delete files by moving to trash
(use-package osx-trash
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  :config (add-hook 'eshell-mode-hook '(lambda ()(exec-path-from-shell-initialize))))

(provide 'setup-osx)
