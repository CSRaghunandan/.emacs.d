;;; setup-css.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-24 11:35:16 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

(defun my-css-mode-hook ()
  (set (make-local-variable 'company-backends)
       '((company-lsp company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev))))

(defun lsp-css-common-setup()
  (lsp)
  (lsp-ui-mode)
  (lsp-ui-doc-mode)
  (my-css-mode-hook)
  (company-mode)
  (eldoc-mode)
  (flycheck-mode)
  (emmet-mode)
  (prettier-js-mode)
  (rainbow-mode))

;; css-mode config
(use-package css-mode
  :ensure nil
  :hook ((css-mode . lsp-css-common-setup))
  :config
  (setq css-indent-offset 2))

(use-package less-css-mode              ; Mode for Less CSS files
  :ensure nil
  :mode "\\.less\\'"
  :hook ((less-css . lsp-css-common-setup)))

;; major mode for editing sass files
;; https://github.com/nex3/sass-mode
(use-package sass-mode
  :mode (("\\.sass\\'" . sass-mode))
  :hook ((sass-mode . lsp-css-common-setup)))

(use-package scss-mode                  ; Mode for SCSS files
  :ensure nil
  :mode "\\.scss\\'"
  :hook ((sass-mode . lsp-css-common-setup)))

(provide 'setup-css)
