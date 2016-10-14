;; Time-stamp: <2016-10-13 18:58:35 csraghunandan>

;; avy, avy-zap

;; avy
;; https://github.com/abo-abo/avy
;; Emacs package for jumping to visible text using character based decision tree
(use-package avy
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?a ?s ?d ?f ?g ?h ?j ?k ?l))))
  (setq avy-style 'pre)
  :bind (("C-'" . avy-goto-char-timer)
          ("C-`" . avy-goto-word-1)))

;; avy-zap
;; https://github.com/cute-jumper/avy-zap
;; Use avy frontend to yank till the match entered by user
(use-package avy-zap
  :bind (("C-c z" . avy-zap-to-char-dwim)))

(provide 'setup-avy)
