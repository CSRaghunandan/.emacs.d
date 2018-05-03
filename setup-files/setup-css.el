;; Time-stamp: <2018-05-04 00:31:16 csraghunandan>

;; css-mode config
(use-package css-mode :defer t
  :ensure nil
  :hook ((css-mode . (lambda ()
                       (rainbow-mode)
                       (my-css-mode-hook)
                       (company-mode)
                       (flycheck-mode))))
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         (>=e "26.0"
             '((company-capf company-files company-yasnippet))
           '((company-css company-files company-yasnippet)))))

  (if (executable-find "prettier")
      (add-hook 'css-mode-hook 'prettier-js-mode)
    (warn "css-mode: prettier not found, automatic formatting of CSS files are disabled")))

(provide 'setup-css)
