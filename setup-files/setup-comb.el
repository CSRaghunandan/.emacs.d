;;; setup-comb.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-03-11 17:27:18 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; https://github.com/cyrus-and/comb
;; Interactive code auditing and grep tool in Emacs Lisp
(use-package comb
  :bind
  (:map comb-keymap
        ("<left>" . comb-prev)
        ("<right>" . comb-next)))

(provide 'setup-comb)
