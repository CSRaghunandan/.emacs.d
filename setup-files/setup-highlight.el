(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
	 ("M-p" . highlight-symbol-prev)
	 ("C-c h s" . highlight-symbol))
  :config
  (highlight-symbol-nav-mode))

;; highlight specific operations like undo, yank
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package hl-line
  :config
  (global-hl-line-mode 1)
  ;; make hl-line highlight only in the current active window
  (setq hl-line-sticky-flag nil))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character ?ا)
  (set-face-foreground 'highlight-indent-guides-character-face "#696969"))

(provide 'setup-highlight)
