(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
	 ("M-p" . highlight-symbol-prev)
	 ("C-c h s" . highlight-symbol))
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 1
        highlight-symbol-on-navigation-p t)
  (highlight-symbol-nav-mode))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package hl-line
  :config
  (global-hl-line-mode 1)
  (setq hl-line-sticky-flag nil))

(use-package highlight-indent-guides :defer t
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character ?ا)
  (set-face-foreground 'highlight-indent-guides-character-face "#696969"))

(provide 'setup-highlight)
