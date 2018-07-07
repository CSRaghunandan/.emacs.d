;; Time-stamp: <2018-07-07 17:48:57 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan rnraghunandan@gmail.com

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
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (add-hook 'css-mode-hook 'prettier-js-mode))

(use-package less-css-mode              ; Mode for Less CSS files
  :ensure nil
  :mode "\\.less\\'")

(use-package scss-mode                  ; Mode for SCSS files
  :ensure nil
  :mode "\\.scss\\'")

;; eldoc-mode plug-in for css-mode
;; https://github.com/zenozeng/css-eldoc/
(use-package css-eldoc
  :commands (turn-on-css-eldoc)
  :hook (css-mode . turn-on-css-eldoc))

(provide 'setup-css)
