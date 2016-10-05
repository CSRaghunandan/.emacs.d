;; configuration for js2-mode

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook 'electric-operator-mode)

  (use-package tern
    :bind* (("C-x t t" . tern-get-type)
            ("C-x t d" . tern-get-docs))
    :config
    (add-hook 'js2-mode-hook 'tern-mode)
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-dabbrev-code company-files company-yasnippet))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (add-hook 'js2-mode-hook 'company-mode))

  (use-package company-tern)

  (use-package js2-refactor
    :diminish js2-refactor
    :bind* ("C-c j r" . js2r-add-keybindings-with-prefix))

  (use-package skewer :defer t))

(provide 'setup-js)
