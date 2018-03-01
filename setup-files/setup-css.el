;; Time-stamp: <2018-03-01 16:48:17 csraghunandan>

;; css-mode config
(use-package css-mode :defer t
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         (>=e "26.0"
             '((company-capf company-files company-yasnippet))
           '((company-css company-files company-yasnippet)))))

  (add-hook 'css-mode-hook 'my-css-mode-hook)
  ;; fontify colors with `rainbow-mode'
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'company-mode)
  (add-hook 'css-mode-hook 'flycheck-mode)

  (if (executable-find "prettier")
      (add-hook 'css-mode-hook 'prettier-js-mode)
    (warn "css-mode: prettier not found, automatic formatting of CSS files are disabled")))

(provide 'setup-css)
