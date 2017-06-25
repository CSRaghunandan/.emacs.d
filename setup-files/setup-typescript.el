;; Time-stamp: <2017-06-26 02:50:34 csraghunandan>

;; typescript config

;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :config

  ;; tide: TypeScript Interactive Development Environment for Emacs
  ;; https://github.com/ananthakumaran/tide
  (use-package tide
    :config
    (bind-key "C-c C-t" 'tide-documentation-at-point typescript-mode-map)

    (defun my-tide-setup-hook ()
      ;; configure tide
      (tide-setup)
      ;; highlight identifiers
      (tide-hl-identifier-mode +1)
      ;;enable eldoc-mode
      (eldoc-mode)
      ;; enable flycheck
      (flycheck-mode)

      ;; format typescript files using prettier
      (prettier-js-mode)

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
                            (xah-clean-whitespace)) nil t)))

    ;; add tslint checker for flycheck
    (flycheck-add-next-checker 'typescript-tide
                               'typescript-tslint)

    ;; use project local tslint versions instead of global
    (defun use-tslint-from-node-modules ()
      (let* ((root (locate-dominating-file
                    (or (buffer-file-name) default-directory)
                    "node_modules"))
             (tslint (and root
                          (expand-file-name (if (eq system-type 'windows-nt)
                                                "node_modules/.bin/tslint.cmd"
                                              "node_modules/.bin/tslint")
                                            root))))
        (when (and tslint (file-executable-p tslint))
          (setq-local flycheck-typescript-tslint-executable tslint))))

    (add-hook 'flycheck-mode-hook #'use-tslint-from-node-modules)))

(provide 'setup-typescript)
