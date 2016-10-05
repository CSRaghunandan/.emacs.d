(use-package python
  :config
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (setq python-shell-interpreter "python3")
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)

  (use-package anaconda-mode
    :bind* (("C-x a d" . anaconda-mode-show-doc))
    :diminish anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

  (use-package company-anaconda
    :config
    (defun my-anaconda-mode-hook ()
      "Hook for `web-mode'."
      (set (make-local-variable 'company-backends)
           '((company-anaconda company-dabbrev-code company-yasnippet company-files))))
    (add-hook 'python-mode-hook 'my-anaconda-mode-hook))

  (use-package pytest :defer t))

(provide 'setup-python)
