;; config for whitespace mode to highlight characters above 80 columns

(use-package whitespace
  :diminish whitespace-mode
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq whitespace-line-column nil) ; When nil, set the value to `fill-column'
  ;; highlight chars above column 80
  (setq whitespace-style '(face lines-tail)))

(provide 'setup-white-space)
