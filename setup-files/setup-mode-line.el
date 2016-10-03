(use-package powerline
  :config (setq powerline-default-separator 'utf-8))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-info-mode)
  (spaceline-toggle-selection-info-off)
  (setq spaceline-toggle-buffer-modified t)
  (spaceline-toggle-buffer-size-off))

(use-package diminish :demand t)

(use-package eldoc
  :diminish eldoc-mode)

(provide 'setup-mode-line)
