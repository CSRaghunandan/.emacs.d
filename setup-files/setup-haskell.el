;;; setup-haskell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-31 22:38:22 csraghunandan>

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
                     (my-haskell-mode-hook)
                     (company-mode)
                     (haskell-collapse-mode))))
  :config

  ;; format and organize imports before save
  (defun lsp-haskell-save-hooks()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'haskell-mode-hook #'lsp-haskell-save-hooks)

  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))))

(provide 'setup-haskell)

;; install haksell by running the command:
;;     curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
;; install haskell-ide-engine using cabal/stack for to use lsp-haskell
;; to use lsp-haskell, create a hie.yaml file in your project root which will
;;     tell GHC how to parse and compile the lib/exec and test files
