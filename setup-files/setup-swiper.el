;;; setup-swiper.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-04-14 15:27:20 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; swiper: isearch with an overview!
;; https://github.com/abo-abo/swiper
(use-package swiper
  :bind (("C-s" . counsel-grep-or-swiper))
  :config
  (setq swiper-action-recenter t
        ;; Jump to the beginning of match when leaving Swiper
        swiper-goto-start-of-match t))

(provide 'setup-swiper)

;; swiper
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
