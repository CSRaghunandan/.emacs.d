;;; setup-rust.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-28 09:54:36 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; rustic-mode: Rust development environment for Emacs
;; only need to install rust-analyzer, no need to install rls or racer for lsp
;; to work for rust
;; https://github.com/brotzeit/rustic
(use-package rustic
 :init
 ;; We use the superior default client provided by `lsp-mode', not the
 ;; one rustic-mode sets up for us
 (setq rustic-lsp-server nil)
 ;; use rust-analyzer instead of rls
 (setq lsp-rust-server 'rust-analyzer)
 ;; disable rustic flycheck error display in modeline. Its redundant
 (setq rustic-flycheck-setup-mode-line-p nil)

  :hook ((rustic-mode . (lambda ()
                        (lsp)
                        (lsp-ui-doc-mode)
                        (lsp-ui-sideline-mode)
                        (lsp-ui-sideline-toggle-symbols-info)
                        (smart-dash-mode)
                        (company-mode))))
  :config
  (setq rust-indent-method-chain t)

  ;; format using rustfmt on save
  (setq rustic-format-on-save t)

  (defun my-rust-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rust-mode-hook))

(provide 'setup-rust)

;; to disable automatic formatting of buffers, put this in `.dir-locals.el'
;; ((rustic-mode
;;   (before-save-hook . (lambda ()
;;                         (time-stamp)
;;                         (ws-butler-after-save)))))
