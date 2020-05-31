;;; setup-json.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-31 22:38:14 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; json-mode: Major mode for editing JSON files with emacs

;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :hook ((json-mode . (lambda ()
                        (lsp-deferred)
                        (company-mode)
                        (flycheck-mode))))
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2)

  (defun my-json-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'json-mode-hook #'my-json-mode-hook))

(provide 'setup-json)
