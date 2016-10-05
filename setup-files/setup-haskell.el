(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-m" . newline)
              ("C-j" . electric-newline-and-maybe-indent))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-intero company-dabbrev company-dabbrev-code company-yasnippet company-files))))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

  (use-package intero
    :diminish intero-mode
    :config (add-hook 'haskell-mode-hook 'intero-mode)))

(provide 'setup-haskell)
