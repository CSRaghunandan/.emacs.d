;; Time-stamp: <2016-11-29 12:59:16 csraghunandan>

;; hungry-delete
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode "h"
  :config
  (global-hungry-delete-mode)
  (add-to-list 'hungry-delete-except-modes 'wdired-mode))

(provide 'setup-hungry-delete)
