;; Time-stamp: <2018-05-05 13:11:53 csraghunandan>

;; typescript config

;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode :defer t
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
      (if (executable-find "prettier")
          (prettier-js-mode)
        (warn "typesecript-mode: prettier executable not found, automatic formatting of .ts files are disabled"))

      ;; company-backends setup
      (set (make-local-variable 'company-backends)
           '((company-tide company-files company-yasnippet))))

    ;; use 2 space indentation
    (setq typescript-indent-level 2)

    (add-hook 'typescript-mode-hook #'my-tide-setup-hook)
    (add-hook 'typescript-mode-hook #'company-mode)

    ;; add tslint checker for flycheck
    (if (executable-find "tslint")
        (flycheck-add-next-checker 'typescript-tide
                                   'typescript-tslint)
      (warn "typescript-mode: tslint not found. Flycheck support for tslint disabled"))

    (add-hook 'typescript-mode-hook 'add-node-modules-path))

  (defun typescript/open-region-in-playground (start end)
    "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                        (url-hexify-string (buffer-substring-no-properties start end)))))
  (bind-key "C-c T p" #'typescript/open-region-in-playground typescript-mode-map))

(provide 'setup-typescript)
