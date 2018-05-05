;; Time-stamp: <2018-05-05 14:39:09 csraghunandan>

;; typescript config

;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode :defer t
  :hook ((typescript-mode . (lambda ()
                              (my-tide-setup-hook)
                              (add-node-modules-path)
                              (company-mode))))
  :bind ((:map typescript-mode-map
               ("C-c C-t" . tide-documentation-at-point)
               ("C-c T p" . typescript/open-region-in-playground)))
  :config
  (defun typescript/open-region-in-playground (start end)
    "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                        (url-hexify-string (buffer-substring-no-properties start end))))))

;; tide: TypeScript Interactive Development Environment for Emacs
;; https://github.com/ananthakumaran/tide
(use-package tide
  :after typescript-mode
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

    ;; format typescript files using prettier
    (if (executable-find "prettier")
        (prettier-js-mode)
      (warn "typesecript-mode: prettier executable not found, automatic formatting of .ts files are disabled"))

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files company-yasnippet))))

  ;; use 2 space indentation
  (setq typescript-indent-level 2)

  ;; add tslint checker for flycheck
  (if (executable-find "tslint")
      (flycheck-add-next-checker 'typescript-tide
                                 'typescript-tslint)
    (warn "typescript-mode: tslint not found. Flycheck support for tslint disabled")))

(provide 'setup-typescript)
