;; Time-stamp: <2018-07-17 16:01:01 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

(defun my-css-mode-hook ()
  (set (make-local-variable 'company-backends)
       '((company-lsp company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev))))

(defun lsp-css-common-setup()
  (lsp-ui-mode)
  (eldoc-mode)
  (flycheck-mode)
  (my-css-mode-hook)
  (company-mode)
  (emmet-mode)
  (prettier-js-mode)
  (rainbow-mode))

;; css-mode config
(use-package css-mode
  :ensure nil
  :hook ((css-mode . (lambda ()
                       (my-css-mode-setup)
                       (lsp-css-common-setup))))
  :config
  (defun my-css-mode-setup ()
    (when (eq major-mode 'css-mode)
      ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
      ;; fires for scss-mode because scss-mode is derived from css-mode)
      (lsp-css-enable)))

  (setq css-indent-offset 2))

;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
;; https://github.com/emacs-lsp/lsp-css
(use-package lsp-css
  :ensure-system-package
  (css-languageserver . "sudo npm i -g vscode-html-languageserver-bin"))

(use-package less-css-mode              ; Mode for Less CSS files
  :ensure nil
  :mode "\\.less\\'"
  :hook ((less-css . (lambda ()
                       (lsp-less-enable)
                       (lsp-css-common-setup)))))

;; major mode for editing sass files
;; https://github.com/nex3/sass-mode
(use-package sass-mode
  :mode (("\\.sass\\'" . sass-mode))
  :hook ((sass-mode . (lambda ()
                        (lsp-scss-enable)
                        (lsp-css-common-setup)))))

(use-package scss-mode                  ; Mode for SCSS files
  :ensure nil
  :mode "\\.scss\\'"
  :hook ((sass-mode . (lambda ()
                        (lsp-scss-enable)
                        (lsp-css-common-setup)))))

(provide 'setup-css)
