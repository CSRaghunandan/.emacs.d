;; Time-stamp: <2016-10-16 17:05:25 csraghunandan>

;; rust-mode, racer, cargo

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :config
  (progn
    (add-hook 'rust-mode-hook 'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
    (add-hook 'rust-mode-hook 'electric-operator-mode)
    (add-hook 'rust-mode-hook
              (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

    ;; cargo-mode for all the cargo related operations
    ;; https://github.com/kwrooijen/cargo.el
    (use-package cargo
      :diminish cargo-mode
      :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

    ;; racer-mode for getting IDE like features for rust-mode
    ;; https://github.com/racer-rust/emacs-racer
    (use-package racer
      :diminish racer-mode
      :bind (("C-c C-t" . racer-describe))
      :config
      (progn
        ;; set racer rust source path environment variable
        (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
        (defun my-racer-mode-hook ()
          (set (make-local-variable 'company-backends)
               '((company-capf company-files company-yasnippet))))
        (add-hook 'racer-mode-hook 'my-racer-mode-hook)
        (add-hook 'racer-mode-hook #'company-mode)
        (add-hook 'rust-mode-hook #'racer-mode)
        (add-hook 'racer-mode-hook #'eldoc-mode)))))

(provide 'setup-rust)
