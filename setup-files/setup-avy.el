;; Time-stamp: <2018-06-07 16:57:23 csraghunandan>

;; avy: package for jumping to visible text using character based decision tree
;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  (("C-`" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line))
  :config (setq avy-style 'pre))

;; ace-link: quickly traverse through links in info
;; https://github.com/abo-abo/ace-link
(use-package ace-link :defer t
  :config
  (ace-link-setup-default)
  ;; add ace-link-org binding to org-mode buffers
  (add-hook 'org-mode-hook
            #'(lambda () (bind-key "C-c M-a" #'ace-link-org org-mode-map))))

(provide 'setup-avy)

;; z - avy action key to zap to character
