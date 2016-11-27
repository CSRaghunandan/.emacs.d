;; Time-stamp: <2016-11-26 23:36:38 csraghunandan>

;; haskell-mode configuration
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  ;; haskell-mode swaps `C-m' and `C-j' behavior. Revert it back
  :bind (:map haskell-mode-map
              ("C-m" . newline)
              ("C-j" . electric-newline-and-maybe-indent))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-intero company-files))))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

  ;; intero-mode for a complete IDE solution to haskell
  ;; commercialhaskell.github.io/intero
  (use-package intero
    :config (add-hook 'haskell-mode-hook 'intero-mode)))

(provide 'setup-haskell)
