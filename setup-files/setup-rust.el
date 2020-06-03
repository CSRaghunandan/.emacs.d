;;; setup-rust.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-03 22:46:58 csraghunandan>

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
  (setq rustic-lsp-server 'rust-analyzer)
  ;; disable rustic flycheck error display in modeline. Its redundant
  (setq rustic-flycheck-setup-mode-line-p nil)

  :hook ((rustic-mode . (lambda ()
                          (lsp-ui-doc-mode)
                          (company-mode))))
  :config
  (setq rust-indent-method-chain t)

  ;; format using rustfmt on save
  (setq rustic-format-on-save t)

  (defun my-rustic-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rustic-mode-hook))

(provide 'setup-rust)

;; to disable automatic formatting of buffers, put this in `.dir-locals.el'
;; ((rustic-mode
;;   (before-save-hook . (lambda ()
;;                         (time-stamp)
;;                         (ws-butler-after-save)))))
