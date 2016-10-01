(use-package avy
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'pre)
  :bind* (("C-'" . avy-goto-char-timer)
	  ("C-`" . avy-goto-word-1)))

(use-package avy-zap
  :bind* (("M-z" . avy-zap-to-char-dwim)
	  ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package ace-window
  :commands (ace-window)
  :bind* ("C-c w" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'setup-avy)
