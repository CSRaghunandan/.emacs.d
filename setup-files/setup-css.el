;; Time-stamp: <2017-09-04 21:38:59 csraghunandan>

;; css-mode config
(use-package css-mode
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-css company-files company-yasnippet))))
  (add-hook 'css-mode-hook 'my-css-mode-hook)
  ;; fontify colors with `rainbow-mode'
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'company-mode)
  (add-hook 'css-mode-hook 'flycheck-mode)

  (if (executable-find "prettier")
      (add-hook 'css-mode-hook 'prettify-js-mode)
    (warn "css-mode: prettier not found, automatic formatting of CSS files are disabled")))

(provide 'setup-css)
