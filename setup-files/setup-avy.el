;;; setup-avy.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-03 00:19:48 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; avy: package for jumping to visible text using character based decision tree
;; https://github.com/abo-abo/avy
(use-package avy
  :bind*
  (("C-'" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line))
  :bind*
  ("C-`" . avy-goto-word-1)
  :config
  (setq avy-style 'pre)
  (setq avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil
        avy-all-windows nil
        avy-all-windows-alt t)
  (setq avy-indent-line-overlay t))

;; ace-link: quickly traverse through links in info
;; https://github.com/abo-abo/ace-link
(use-package ace-link
  :config
  (ace-link-setup-default))

(provide 'setup-avy)

;; z - avy action key to zap to character
