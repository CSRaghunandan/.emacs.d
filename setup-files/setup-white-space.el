;; Time-stamp: <2016-10-06 22:21:09 csraghunandan>

;; whitespace
;; config for whitespace mode to highlight characters above 80 columns
(use-package whitespace :defer t
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column nil) ; When nil, set the value to `fill-column'
  ;; highlight chars above column 80
  (setq whitespace-style '(face lines-tail)))

(provide 'setup-white-space)
