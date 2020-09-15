;;; setup-lsp.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-09-15 10:39:04 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; lsp-mode:  Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands lsp lsp-deferred
  :hook ((lsp-after-open . lsp-enable-imenu)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-after-open . (lambda ()
                            (setq-local company-minimum-prefix-length 1
                                  company-idle-delay 0.0) ;; default is 0.2
                            )))
  :bind (:map lsp-mode-map
              ("C-c C-t" . lsp-describe-thing-at-point))
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-prefer-capf t
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil))

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature t
        lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
        lsp-ui-doc-position 'at-point
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))

;; debugger adapter protocol support for emacs
;; https://github.com/emacs-lsp/dap-mode/
(use-package dap-mode
  :defer 4
  :config
  ;; call dap-hydra when going to the next breakpoint
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (add-hook 'dap-mode-hook #'dap-ui-mode) ; use a hook so users can remove it
  (dap-mode 1))

;; load gdb-lldb package
(use-package dap-gdb-lldb
  :defer 5
  :straight nil)

(provide 'setup-lsp)
