;; Time-stamp: <2017-09-04 20:48:46 csraghunandan>

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
    (add-hook 'typescript-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda()
                            (time-stamp)
                            (xah-clean-whitespace)) nil t)))

    ;; add tslint checker for flycheck
    (if (executable-find "tslint")
        (flycheck-add-next-checker 'typescript-tide
                                   'typescript-tslint)
      (warn "typescript-mode: tslint not found. Flycheck support for tslint disabled"))

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

    (add-hook 'flycheck-mode-hook #'use-tslint-from-node-modules))

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
