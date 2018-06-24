;; Time-stamp: <2018-06-24 10:07:33 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; avy: package for jumping to visible text using character based decision tree
;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  (("C-`" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)
   ("M-g [" . avy-goto-paren-open)
   ("M-g ]" . avy-goto-paren-close))
  :config
  (setq avy-style 'pre)

  (defun avy-goto-paren-open ()
    (interactive)
    (avy--generic-jump "(\\|{\\|[" nil 'pre))

  (defun avy-goto-paren-close ()
    (interactive)
    (avy--generic-jump ")\\|}\\|]" nil 'pre)))

;; ace-link: quickly traverse through links in info
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :config
  (ace-link-setup-default))

(provide 'setup-avy)

;; z - avy action key to zap to character
