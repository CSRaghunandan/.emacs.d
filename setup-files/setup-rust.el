;; Time-stamp: <2018-01-31 12:49:32 csraghunandan>

;; rust-mode, racer, cargo

;; rust-mode: major-mode for editing rust files
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode :defer t
  :config
  ;; add flycheck support for rust
  ;; https://github.com/flycheck/flycheck-rust
  (use-package flycheck-rust)

  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil))

  ;; cargo-mode: execute cargo commands easily
  ;; https://github.com/kwrooijen/cargo.el
  (use-package cargo
    :diminish (cargo-minor-mode . "𝐂𝐚")
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

  ;; racer: autocompletions/jump to definitions and eldoc support
  ;; https://github.com/racer-rust/emacs-racer
  (use-package racer
    :if (executable-find "racer")
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

  ;; ;; format rust buffers using rustfmt(if it is installed)
  ;; (if (executable-find "rustfmt")
  ;;   (add-hook 'rust-mode-hook
  ;;             (lambda ()
  ;;               (add-hook 'before-save-hook
  ;;                         (lambda ()
  ;;                           (time-stamp)
  ;;                           (xah-clean-whitespace)
  ;;                           (rust-format-buffer)) nil t)))
  ;;   (warn "rust-mode: rustfmt not foud, automatic source code formatting disabled"))

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
   ("C-c v s" . wh/rust-vec-as-slice)))

(provide 'setup-rust)

;; to disable automatic formatting of buffers, put this in `.dir-locals.el'
;; ((rust-mode
;;   (before-save-hook . (lambda ()
;;                         (time-stamp)
;;                         (ws-butler-after-save)))))
