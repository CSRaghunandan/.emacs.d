;; Time-stamp: <2016-10-07 12:06:51 csraghunandan>

;; hungry-delete
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :diminish hungry-delete-mode "h"
  :config
  (global-hungry-delete-mode)
  (setq hungry-delete-chars-to-skip " \t\r\f\v")

  (defun turn-off-hungry-delete-mode ()
    "Turn off hungry delete mode."
    (hungry-delete-mode -1))

  ;; Except ..
  ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
  ;; i.e. when editing file names in the *Dired* buffer.
  (add-hook 'wdired-mode-hook #'turn-off-hungry-delete-mode))

(provide 'setup-hungry-delete)
