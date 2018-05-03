;; Time-stamp: <2018-05-04 00:09:01 csraghunandan>

;; lsp-mode:  Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :config (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

;; company-lsp: Company completion backend for lsp-mode.
;; https://github.com/tigersoldier/company-lsp/
(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t))

(provide 'setup-lsp)
