;; Time-stamp: <2016-10-17 21:02:54 csraghunandan>

;; ehsell config
(use-package eshell
  :config
  ;; get ivy/helm completions to work in `eshell'
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                (lambda () (interactive) (pcomplete-std-complete)))))
  ;; fetch the $PATH variable to eshell
  (add-hook 'eshell-mode-hook '(lambda ()(exec-path-from-shell-initialize))))

(provide 'setup-eshell)
