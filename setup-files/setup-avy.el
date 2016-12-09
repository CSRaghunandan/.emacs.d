;; Time-stamp: <2016-12-09 12:12:56 csraghunandan>

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
  :bind* (("M-z" . avy-zap-to-char-dwim)))

;; ace-link
;; https://github.com/abo-abo/ace-link
;; quickly traverse through links in buffers
(use-package ace-link
  :config (ace-link-setup-default))

(provide 'setup-avy)
