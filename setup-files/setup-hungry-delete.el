;; Time-stamp: <2016-11-28 16:21:30 csraghunandan>

;; hungry-delete
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode "h"
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-chars-to-skip " \t\r\f\v")
  (add-to-list 'hungry-delete-except-modes '(wdired-mode)))

(provide 'setup-hungry-delete)
