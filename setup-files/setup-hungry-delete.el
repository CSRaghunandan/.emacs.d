;; Time-stamp: <2018-06-04 14:18:41 csraghunandan>

;; hungry-delete: deleting a whitespace character will delete all whitespace
;; until the next non-whitespace character
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode)
  (add-to-list 'hungry-delete-except-modes 'wdired-mode)
  (add-to-list 'hungry-delete-except-modes 'ivy-occur-mode))

(provide 'setup-hungry-delete)
