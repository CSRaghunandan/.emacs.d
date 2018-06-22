;; Time-stamp: <2018-06-22 12:22:41 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; rust-mode, RLS, cargo

;; lsp-rust: Rust support for lsp-mode using the Rust Language Server.
;; https://github.com/emacs-lsp/lsp-rust
(use-package lsp-rust)

;; rust-mode: major-mode for editing rust files
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :hook ((rust-mode . (lambda ()
                        (lsp-rust-enable)
                        (lsp-ui-mode)
                        (eldoc-mode)
                        (flycheck-mode)
                        (smart-dash-mode)
                        (company-mode))))

  :bind (:map rust-mode-map
         ("C-c v t" . wh/rust-toggle-visibility)
         ("C-c m t" . wh/rust-toggle-mutability)
         ("C-c v s" . wh/rust-vec-as-slice))
  :ensure-system-package
  ((rustfmt . "rustup component add rustfmt-preview")
   (racer . "cargo install racer")
   (rls . "rustup component add rls-preview rust-analysis rust-src"))
  :config
  (setq rust-indent-method-chain t)

  (defun my-rust-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-yasnippet company-files))))
  (add-hook 'rust-mode-hook #'my-rust-mode-hook)

  ;; format rust buffers using rustfmt(if it is installed)
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (time-stamp)
                          (lsp-format-buffer)) nil t)))

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

;; cargo-mode: execute cargo commands easily
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode)))

(provide 'setup-rust)

;; to disable automatic formatting of buffers, put this in `.dir-locals.el'
;; ((rust-mode
;;   (before-save-hook . (lambda ()
;;                         (time-stamp)
;;                         (ws-butler-after-save)))))
