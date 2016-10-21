;; Time-stamp: <2016-10-21 09:36:23 csraghunandan>

;; ehsell config
(use-package eshell
  :config
  ;; get ivy/helm completions to work in `eshell'
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                'completion-at-point)))
  ;; fetch the $PATH variable to eshell
  (add-hook 'eshell-mode-hook '(lambda ()(exec-path-from-shell-initialize))))

(provide 'setup-eshell)
