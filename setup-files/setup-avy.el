;; Time-stamp: <2016-10-06 22:32:29 csraghunandan>

;; avy, avy-zap and ace-window

;; avy
;; https://github.com/abo-abo/avy
;; Emacs package for jumping to visible text using character based decision tree
(use-package avy
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?a ?s ?d ?f ?g ?h ?j ?k ?l))))
  (setq avy-style 'pre)
  :bind* (("C-'" . avy-goto-char-timer)
	  ("C-`" . avy-goto-word-1)))

;; avy-zap
;; https://github.com/cute-jumper/avy-zap
;; Use avy frontend to yank till the match entered by user
(use-package avy-zap
  :bind* (("M-z" . avy-zap-to-char-dwim)
	  ("M-Z" . avy-zap-up-to-char-dwim)))

;; ace-window
;; https://github.com/abo-abo/ace-window
;; allows for quick switching of windows within the current emacs frame
(use-package ace-window
  :commands (ace-window)
  :bind* ("C-c w" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'setup-avy)
