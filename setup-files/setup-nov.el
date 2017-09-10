;; Time-stamp: <2017-09-10 08:31:54 csraghunandan>

;; nov: Epub reader for emacs
;; https://github.com/wasamasa/nov.el
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width most-positive-fixnum)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode))

(provide 'setup-nov)
