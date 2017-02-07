;; Time-stamp: <2017-02-07 19:37:10 csraghunandan>

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

  ;; format CSS buffers on save
  (when (executable-find "js-beautify")
    (add-hook 'css-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (time-stamp)
                            (web-beautify-css-buffer)) nil t)))))

(provide 'setup-css)
