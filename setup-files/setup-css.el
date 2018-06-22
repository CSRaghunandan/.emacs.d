;; Time-stamp: <2018-06-22 11:55:19 csraghunandan>

;; css-mode config
(use-package css-mode
  :ensure nil
  :mode (("\\.sass\\'" . css-mode))
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

(use-package less-css-mode              ; Mode for Less CSS files
  :mode "\\.less\\'")

(use-package scss-mode                  ; Mode for SCSS files
  :mode "\\.scss\\'")

;; eldoc-mode plug-in for css-mode
;; https://github.com/zenozeng/css-eldoc/
(use-package css-eldoc
  :commands (turn-on-css-eldoc)
  :hook (css-mode . turn-on-css-eldoc))

(provide 'setup-css)
