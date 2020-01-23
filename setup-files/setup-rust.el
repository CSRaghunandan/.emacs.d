;;; setup-rust.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-23 13:56:30 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; rustic-mode: Rust development environment for Emacs
;; https://github.com/brotzeit/rustic
(use-package rustic
 :init
 ;; use rust-analyzer for lsp
 (setq rustic-lsp-server nil)
 ;; set path for rust-analyzer
 ;; (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/ra_lsp_server"))
 (setq lsp-rust-server 'rust-analyzer)
 ;; disable rustic flycheck error display in modeline. Its redundant
 (setq rustic-flycheck-setup-mode-line-p nil)

  :hook ((rustic-mode . (lambda ()
                        (lsp)
                        (lsp-ui-doc-mode)
                        (lsp-ui-sideline-mode)
                        (lsp-ui-sideline-toggle-symbols-info)
                        (flycheck-mode)
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
