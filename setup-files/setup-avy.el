;; Time-stamp: <2017-01-08 11:55:35 csraghunandan>

;; avy, avy-zap

;; avy - Emacs package for jumping to visible text using character based decision tree
;; https://github.com/abo-abo/avy
(use-package avy
  :bind ("C-`" . avy-goto-word-1)
  :bind* ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?a ?s ?d ?f ?g ?h ?j ?k ?l))))
  (setq avy-style 'pre))

;; avy-zap - Use avy frontend to yank till the match entered by user
;; https://github.com/cute-jumper/avy-zap
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim))
  :config (advice-add 'swiper :before 'avy-push-mark))

;; ace-link - quickly traverse through links in info
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :config (ace-link-setup-default))

(provide 'setup-avy)
