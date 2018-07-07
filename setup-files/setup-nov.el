;; Time-stamp: <2018-07-07 18:07:14 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; nov: Epub reader for emacs
;; https://github.com/wasamasa/nov.el
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode))
  :config
  (setq nov-text-width most-positive-fixnum)
  (setq visual-fill-column-center-text t))

(provide 'setup-nov)
