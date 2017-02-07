;; Time-stamp: <2017-02-07 15:00:02 csraghunandan>

;; typescript config

;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :config

  ;; tide: TypeScript Interactive Development Environment for Emacs
  ;; https://github.com/ananthakumaran/tide
  (use-package tide
    :config

    (defun my-tide-setup-hook ()
      ;; configure tide
      (tide-setup)
      ;; highlight identifiers
      (tide-hl-identifier-mode +1)
      ;;enable eldoc-mode
      (eldoc-mode)
      ;; enable flycheck
      (flycheck-mode)

      ;; company-backends setup
      (set (make-local-variable 'company-backends)
           '((company-tide company-files company-yasnippet))))

    (add-hook 'typescript-mode-hook #'my-tide-setup-hook)
    (add-hook 'typescript-mode-hook #'company-mode)
    (add-hook 'typescript-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda()
                            (time-stamp)
                            (tide-format-before-save)) nil t)))

    ;; add tslint checker for flycheck
    (flycheck-add-next-checker 'typescript-tide
                               'typescript-tslint)))

(provide 'setup-typescript)
