;; Time-stamp: <2018-06-11 00:01:22 csraghunandan>

;; css-mode config
(use-package css-mode
  :ensure nil
  :mode (("\\.scss\\'" . css-mode)
         ("\\.sass\\'" . css-mode))
  :hook ((css-mode . (lambda ()
                       (rainbow-mode)
                       (my-css-mode-hook)
                       (company-mode)
                       (flycheck-mode)
                       (emmet-mode))))
  :config
  (setq css-indent-offset 2)

  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         (>=e "26.0"
             '((company-capf company-files company-yasnippet))
           '((company-css company-files company-yasnippet)))))

  (add-hook 'css-mode-hook 'prettier-js-mode))

(provide 'setup-css)
