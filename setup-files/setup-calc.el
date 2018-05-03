;; Time-stamp: <2017-02-08 11:19:20 csraghunandan>

;; calc config
(use-package calc
  :ensure nil
  :bind (("C-x c" . calc)
          ("C-x ," . quick-calc))
  :config
  (setq calc-twos-complement-mode nil)
  ;; Calculator output value format
  (setq calc-float-format '(eng 4))) ; Engineering notation

(provide 'setup-calc)
