;; Time-stamp: <2018-07-07 18:07:27 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

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
