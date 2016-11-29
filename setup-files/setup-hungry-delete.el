;; Time-stamp: <2016-11-29 10:46:15 csraghunandan>

;; hungry-delete
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode "h"
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-chars-to-skip " \t\r\f\v")

  (defun modi/turn-off-hungry-delete-mode ()
    "Turn off hungry delete mode."
    (hungry-delete-mode -1))

  (add-hook 'wdired-mode-hook #'modi/turn-off-hungry-delete-mode)
  (add-hook 'minibuffer-setup-hook #'modi/turn-off-hungry-delete-mode))

(provide 'setup-hungry-delete)
