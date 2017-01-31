;; Time-stamp: <2017-01-31 12:42:21 csraghunandan>

;; rust-mode, racer, cargo

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :config
  ;; add flycheck support for rust
  ;; https://github.com/flycheck/flycheck-rust
  (use-package flycheck-rust)

  ;; cargo-mode for all the cargo related operations
  ;; https://github.com/kwrooijen/cargo.el
  (use-package cargo
    :diminish (cargo-minor-mode . "𝐂𝐚")
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

  ;; racer-mode for getting IDE like features for rust-mode
  ;; https://github.com/racer-rust/emacs-racer
  (use-package racer
    :diminish racer-mode "𝐑𝐚"
    :bind (:map rust-mode-map
                (("C-c C-t" . racer-describe)))
    :config
    (defun my-racer-mode-hook ()
      (set (make-local-variable 'company-backends)
           '((company-capf company-files company-yasnippet))))

    ;; enable company and eldoc minor modes in rust-mode
    (add-hook 'racer-mode-hook #'my-racer-mode-hook)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode))

  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'smart-dash-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'racer-mode)

  ;; format rust buffers using rustfmt(if it is installed)
  (when (executable-find "rustfmt")
    (add-hook 'rust-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (time-stamp)
                            (xah-clean-whitespace)
                            (rust-format-buffer)) nil t))))

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
    (insert "[..]"))

  (bind-keys
   :map rust-mode-map
   ("C-c v t" . wh/rust-toggle-visibility)
   ("C-c m t" . wh/rust-toggle-mutability)
   ("C-c v s" . wh/rust-vec-as-slice))

  ;; rust-playground
  ;; https://github.com/grafov/rust-playground
  (use-package rust-playground
    :bind (("C-c r p" . rust-playground))
    :config
    (setq rust-playground-basedir "~/Code-files/rust/playground/")))

(provide 'setup-rust)
