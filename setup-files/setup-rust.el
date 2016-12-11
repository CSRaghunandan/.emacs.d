;; Time-stamp: <2016-12-12 03:26:53 csraghunandan>

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
    ;; set racer rust source path environment variable
    (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
    (defun my-racer-mode-hook ()
      (set (make-local-variable 'company-backends)
           '((company-capf company-files))))

    ;; enable company and eldoc minor modes in rust-mode
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode))

  (add-hook 'rust-mode-hook 'flycheck-mode)
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
                            (rust-format-buffer)
                            (force-backup-of-buffer)) nil t)))))

(provide 'setup-rust)
