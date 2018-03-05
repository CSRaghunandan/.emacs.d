;; TIme-stamp: <2017-01-05 15:49:39>

;; ehsell config
(use-package eshell
  :config

  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys
               :map eshell-mode-map
               ("<tab>" . completion-at-point)
               ("C-c M-o" . eshell-clear-buffer))))

  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

(provide 'setup-eshell)
