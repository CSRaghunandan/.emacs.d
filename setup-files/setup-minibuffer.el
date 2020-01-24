;;; setup-minibuffer.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 03:04:06 csraghunandan>

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

(defun my-minibuffer-setup-hook ()
  "disable whole-line-or-region inside minibuffer"
  (whole-line-or-region-local-mode -1))
(defun my-minibuffer-exit-hook ()
  "enable it again after exiting minibuffer"
  (whole-line-or-region-local-mode))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; enable some minor modes when in eval-expression of minibuffer
(add-hook 'eval-expression-minibuffer-setup-hook (lambda()
                                                   (smartparens-mode)
                                                   (rainbow-delimiters-mode)
                                                   (eldoc-mode)))

(provide 'setup-minibuffer)
