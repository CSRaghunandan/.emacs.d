;;; setup-rust.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-14 13:44:46 csraghunandan>

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

  :hook ((rustic-mode . (lambda ()
                        (lsp)
                        (lsp-ui-doc-mode)
                        (lsp-ui-sideline-mode)
                        (lsp-ui-sideline-toggle-symbols-info)
                        (flycheck-mode)
                        (smart-dash-mode)
                        (company-mode))))

  :bind (:map rustic-mode-map
         ("C-c v t" . wh/rust-toggle-visibility)
         ("C-c m t" . wh/rust-toggle-mutability)
         ("C-c v s" . wh/rust-vec-as-slice))
  :config
  (setq rust-indent-method-chain t)

  ;; format using rustfmt on save
  (setq rustic-format-on-save t)

  (defun my-rust-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rust-mode-hook)

  (defun wh/rust-toggle-mutability ()
    "Toggle the mutability of the variable at point."
    (interactive)
    (save-excursion
      (racer-find-definition)
      (back-to-indentation)
      (forward-char 4)
      (if (looking-at "mut ")
          (delete-char 4)
        (insert "mut "))))

  (defun wh/rust-toggle-visibility ()
    "Toggle the public visibility of the function at point."
    (interactive)
    (save-excursion
      ;; If we're already at the beginning of the function definition,
      ;; `beginning-of-defun' moves to the previous function, so move elsewhere.
      (end-of-line)

      (beginning-of-defun)
      (if (looking-at "pub ")
          (delete-char 4)
        (insert "pub "))))

  (defun wh/rust-vec-as-slice ()
    "Convert the vector expression at point to a slice.
foo -> &foo[..]"
    (interactive)
    (insert "&")
    (forward-symbol 1)
    (insert "[..]")))

(provide 'setup-rust)

;; to disable automatic formatting of buffers, put this in `.dir-locals.el'
;; ((rustic-mode
;;   (before-save-hook . (lambda ()
;;                         (time-stamp)
;;                         (ws-butler-after-save)))))
