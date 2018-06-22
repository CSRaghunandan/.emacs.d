;; Time-stamp: <2018-06-22 12:11:31 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

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
