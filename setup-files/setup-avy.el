;; Time-stamp: <2018-06-11 01:49:56 csraghunandan>

;; avy: package for jumping to visible text using character based decision tree
;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  (("C-`" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line))
  :custom
  (avy-style 'pre))

;; ace-link: quickly traverse through links in info
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :config
  (ace-link-setup-default))

(provide 'setup-avy)

;; z - avy action key to zap to character
