;;; setup-calc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:41:49 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; calc config
(use-package calc
  :straight nil
  :bind (("C-x c" . calc)
         ("C-x ," . quick-calc))
  :config
  (setq calc-twos-complement-mode nil)
  ;; Engineering notation
  (setq calc-float-format '(eng 4)))

(provide 'setup-calc)
