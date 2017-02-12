;; Time-stamp: <2017-02-12 13:42:08 csraghunandan>

;; hungry-delete: deleting a whitespace character will delete all whitespace
;; until the next non-whitespace character
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode "𝐡"
  :config
  (global-hungry-delete-mode)
  (add-to-list 'hungry-delete-except-modes 'wdired-mode))

(provide 'setup-hungry-delete)
