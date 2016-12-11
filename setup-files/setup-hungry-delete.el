;; Time-stamp: <2016-12-12 02:40:39 csraghunandan>

;; hungry-delete
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode "𝐡"
  :config
  (global-hungry-delete-mode)
  (add-to-list 'hungry-delete-except-modes 'wdired-mode))

(provide 'setup-hungry-delete)
