(use-package calc :defer t
  :bind* (("C-x c" . calc)
	  ("C-x C" . quick-calc))
  :config
  (setq calc-twos-complement-mode nil)
  ;; Calculator output value format
  (setq calc-float-format '(eng 4))) ; Engineering notation

(provide 'setup-calc)
