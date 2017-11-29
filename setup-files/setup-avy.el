;; Time-stamp: <2017-11-29 20:01:56 csraghunandan>

;; avy: package for jumping to visible text using character based decision tree
;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  (("C-`" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer))
  :config

  ;; use home row keys for avy jumps
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?a ?s ?d ?f ?g ?h ?j ?k ?l))))
  (setq avy-style 'pre))

;; avy-zap: Use avy frontend to yank till the match entered by user
;; https://github.com/cute-jumper/avy-zap
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)))

;; ace-link: quickly traverse through links in info
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :config (ace-link-setup-default))

(provide 'setup-avy)
