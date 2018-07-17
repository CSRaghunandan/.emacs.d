;; Time-stamp: <2018-07-17 18:12:53 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; HTML support for lsp-mode using vscode-html-languageserver-bin
;; https://github.com/emacs-lsp/lsp-html
(use-package lsp-html
  :ensure-system-package
  (html-languageserver . "npm i -g vscode-html-languageserver-bin"))

;; web-mode: major-mode for editing multiple web formats
;; http://web-mode.org/ , https://github.com/fxbois/web-mode
(use-package web-mode
  :mode (("\\.html$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :bind (:map web-mode-map
         ("C-c o b" . browse-url-of-file)
         ("C-c [" . emmet-prev-edit-point)
         ("C-c ]" . emmet-next-edit-point))
  :hook ((web-mode . company-mode))
  :config

  (custom-set-variables
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(css-indent-offset 2))

  ;; highlight matching tag
  (setq web-mode-enable-current-element-highlight t)

  (defun my-unfontify-function (beg end)
    (remove-list-of-text-properties beg end '(display)))
  (defun my-register-unfontify ()
    (setq font-lock-unfontify-region-function 'my-unfontify-function))
  (add-hook 'web-mode-hook 'my-register-unfontify t)

  (defun my-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;; enable flycheck
    (flycheck-mode)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))

    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  (defun my-web-mode-hook ()
    "company hook for `web-mode' for non-html buffers."
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (defun my-lsp-html-mode-hook ()
    " company hook for `web-mode' for html buffers."
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (defun lsp-html-setup ()
    "Function to setup `lsp-html'"
    (lsp-html-enable)
    (lsp-ui-mode)
    (my-lsp-html-mode-hook)
    (setq-local lsp-highlight-symbol-at-point nil))

  (add-hook 'web-mode-hook
            (lambda ()
              (pcase (file-name-extension buffer-file-name)
                ("tsx" (my-tide-setup-hook))
                ("html" (lsp-html-setup))
                (_ (my-web-mode-hook)))))

  ;; colorize colors in buffers
  (setq web-mode-enable-css-colorization t))

;; impatient mode: Live refresh of web pages
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :commands (impatient-mode))

;; emmet-mode: dynamic snippets for HTML
;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :hook ((web-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-indentation 2))

(provide 'setup-web-mode)
