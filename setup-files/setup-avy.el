;; Time-stamp: <2018-03-05 14:23:05 csraghunandan>

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
  :config
  (ace-link-setup-default)
  ;; add ace-link-org binding to org-mode buffers
  (add-hook 'org-mode-hook
            #'(lambda () (bind-key "C-c M-a" #'ace-link-org org-mode-map))))

(provide 'setup-avy)
