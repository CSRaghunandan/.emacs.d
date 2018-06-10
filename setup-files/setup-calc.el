;; Time-stamp: <2018-06-11 01:58:25 csraghunandan>

;; calc config
(use-package calc
  :ensure nil
  :bind (("C-x c" . calc)
         ("C-x ," . quick-calc))
  :custom
  (calc-twos-complement-mode nil)
  (calc-float-format '(eng 4) "Engineering notation"))

(provide 'setup-calc)
