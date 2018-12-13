;;; setup-css.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-12-14 01:14:09 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

(defun my-css-mode-hook ()
  (set (make-local-variable 'company-backends)
       '((company-lsp company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev))))

(defun lsp-css-common-setup()
  (eldoc-mode)
  (flycheck-mode)
  (company-mode)
  (emmet-mode)
  (prettier-js-mode)
  (rainbow-mode))

;; css-mode config
(use-package css-mode
  :ensure nil
  :hook ((css-mode . (lambda ()
                       (lsp-css-common-setup)
                       (lsp))))
  :config
  (setq css-indent-offset 2))

(use-package less-css-mode              ; Mode for Less CSS files
  :ensure nil
  :mode "\\.less\\'"
  :hook ((less-css . (lambda ()
                       (lsp-css-common-setup)
                       (lsp)))))

;; major mode for editing sass files
;; https://github.com/nex3/sass-mode
(use-package sass-mode
  :mode (("\\.sass\\'" . sass-mode))
  :hook ((sass-mode . (lambda ()
                        (lsp-css-common-setup)
                        (lsp)))))

(use-package scss-mode                  ; Mode for SCSS files
  :ensure nil
  :mode "\\.scss\\'"
  :hook ((sass-mode . (lambda ()
                        (lsp-css-common-setup)
                        (lsp)))))

(provide 'setup-css)
