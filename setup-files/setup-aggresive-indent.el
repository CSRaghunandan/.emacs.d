(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode)
  (global-aggressive-indent-mode))

(provide 'setup-aggresive-indent)
