;; Time-stamp: <2018-07-17 13:26:28 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; css-mode config
(use-package css-mode
  :ensure nil
  :hook ((css-mode . (lambda ()
                       (rainbow-mode)
                       (lsp-css-mode-setup)
                       (my-css-mode-hook)
                       (company-mode)
                       (emmet-mode)
                       (prettier-js-mode))))
  :config

  (defun lsp-css-mode-setup ()
    (when (eq major-mode 'css-mode)
      ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
      ;; fires for scss-mode because scss-mode is derived from css-mode)
      (lsp-css-enable)
      (lsp-ui-mode)
      (eldoc-mode)
      (flycheck-mode)))

  (setq css-indent-offset 2)

  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))))

;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
;; https://github.com/emacs-lsp/lsp-css
(use-package lsp-css
  :after css-mode)

(use-package less-css-mode              ; Mode for Less CSS files
  :ensure nil
  :mode "\\.less\\'")

;; major mode for editing sass files
;; https://github.com/nex3/sass-mode
(use-package sass-mode
  :mode (("\\.sass\\'" . sass-mode)))

(use-package scss-mode                  ; Mode for SCSS files
  :ensure nil
  :mode "\\.scss\\'")

(provide 'setup-css)
