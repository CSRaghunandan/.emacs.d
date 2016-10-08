;; Time-stamp: <2016-10-09 00:39:44 csraghunandan>

;; All the highlight stuff config

;; highlight the currently active symbol + move to next/previous occurrence of symbol
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
	 ("M-p" . highlight-symbol-prev)
	 ("C-c h s" . highlight-symbol))
  :config
  (highlight-symbol-nav-mode))

;; highlight specific operations like undo, yank
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; configure hl-line-mode
(use-package hl-line
  :config
  (global-hl-line-mode)
  ;; make hl-line highlight only in the current active window
  (setq hl-line-sticky-flag nil))

;; best solution for highlighting indent guides so far in emacs
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character ?ا)
  (set-face-foreground 'highlight-indent-guides-character-face "#696969"))

(provide 'setup-highlight)
