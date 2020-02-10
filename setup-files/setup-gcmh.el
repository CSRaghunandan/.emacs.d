;;; setup-gcmh.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-10 11:35:10 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; GCMH - the Garbage Collector Magic Hack
;; https://gitlab.com/koral/gcmh
(use-package gcmh
  :config
  (gcmh-mode +1)
  (setq gcmh-idle-delay 10 ; garbage collect after 10s of idle time
        gcmh-high-cons-threshold 16777216) ; 16mb

  ;; garbage collect when moving out to other applications
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))

(provide 'setup-gcmh)
