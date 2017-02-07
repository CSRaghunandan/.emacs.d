;; Time-stamp: <2017-02-07 11:54:15 csraghunandan>

;; typescript config

;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :config

  (add-hook 'typescript-mode-hook #'flycheck-mode)
  (add-hook 'typescript-mode-hook #'eldoc-mode)

  ;; tide: TypeScript Interactive Development Environment for Emacs
  ;; https://github.com/ananthakumaran/tide
  (use-package tide
    :config

    (defun my-typescript-mode-hook ()
      ;; configure tide
      (tide-setup)
      ;; highlight identifiers
      (tide-hl-identifier-mode +1)
      ;; company-backends setup
      (set (make-local-variable 'company-backends)
           '((company-tide company-files company-yasnippet))))

    (add-hook 'typescript-mode-hook #'my-typescript-mode-hook)
    (add-hook 'typescript-mode-hook #'company-mode)
    (add-hook 'typescript-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda()
                            (time-stamp)
                            (tide-format-before-save)) nil t)))))

(provide 'setup-typescript)
