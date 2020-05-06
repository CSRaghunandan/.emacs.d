;;; setup-haskell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-06 20:34:20 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; LSP integration for haskell
;; https://emacs-lsp.github.io/lsp-haskell/
(use-package lsp-haskell)

;; haskell-mode: major mode for editing haskell files
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :hook
  ((haskell-mode . (lambda ()
                     (lsp)
                     (direnv-update-environment)
                     (lsp-ui-doc-mode)
                     (lsp-ui-sideline-mode)
                     (lsp-ui-sideline-toggle-symbols-info)
                     (my-haskell-mode-hook)
                     (company-mode)
                     (haskell-collapse-mode))))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))))

(provide 'setup-haskell)
