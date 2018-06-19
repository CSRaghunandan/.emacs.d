;; Time-stamp: <2018-06-19 11:06:23 csraghunandan>

;; calc config
(use-package calc
  :ensure nil
  :bind (("C-x c" . calc)
         ("C-x ," . quick-calc))
  :config
  (setq calc-twos-complement-mode nil)
  ;; Engineering notation
  (setq calc-float-format '(eng 4)))

(provide 'setup-calc)
