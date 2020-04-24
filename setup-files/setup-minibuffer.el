;;; setup-minibuffer.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-04-24 22:43:59 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; configuration for minibuffer

;; http://oremacs.com/2016/06/06/counsel-set-variable/
(when (not (bound-and-true-p disable-recursive-edit-in-minibuffer))
  ;; Allow to read from minibuffer while in minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; Show the minibuffer depth (when larger than 1)
  (minibuffer-depth-indicate-mode 1))

;; resize minibuffer window to accommodate text
(setq resize-mini-window t)

;; enable some minor modes when in eval-expression of minibuffer
(add-hook 'eval-expression-minibuffer-setup-hook (lambda()
                                                   (smartparens-mode)
                                                   (rainbow-delimiters-mode)
                                                   (eldoc-mode)))

(provide 'setup-minibuffer)
