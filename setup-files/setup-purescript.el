;;; setup-purescript.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-31 22:38:27 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; configuration for purescript

;; purescript-mode: major mode for editing purescript files
;; https://github.com/dysinger/purescript-mode
(use-package purescript-mode :defer t
  :hook ((purescript-mode . turn-on-purescript-decl-scan)
         (purescript-mode . (lambda ()
                              (lsp-deferred)
                              (company-mode))))
  :config
  (defun my-purescript-mode-hook()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'purescript-mode-hook #'my-purescript-mode-hook))

(provide 'setup-purescript)
