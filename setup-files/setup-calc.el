;; Time-stamp: <2016-12-12 22:39:47 csraghunandan>

;; calc config
(use-package calc :defer t
  :bind* (("C-x c" . calc)
          ("C-x ," . quick-calc))
  :config
  (setq calc-twos-complement-mode nil)
  ;; Calculator output value format
  (setq calc-float-format '(eng 4))) ; Engineering notation

(provide 'setup-calc)
