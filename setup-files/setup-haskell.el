;; Time-stamp: <2016-10-09 19:31:10 csraghunandan>

;; haskell-mode configuration
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-m" . newline)
              ("C-j" . electric-newline-and-maybe-indent))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-intero company-dabbrev-code company-yasnippet company-files))))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

  ;; intero-mode for a complete IDE solution to haskell
  ;; commercialhaskell.github.io/intero
  (use-package intero
    :diminish intero-mode
    :config (add-hook 'haskell-mode-hook 'intero-mode)))

(provide 'setup-haskell)
