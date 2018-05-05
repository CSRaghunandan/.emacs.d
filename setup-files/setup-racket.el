;; Time-stamp: <2018-05-05 18:16:29 csraghunandan>

;; racket-mode: major mode for editing racket files
;; https://github.com/greghendershott/racket-mode
(use-package racket-mode :defer t
  :hook ((racket-mode . (lambda ()
                          (my-racket-mode-hook)
                          (company-mode)))
         (racket-repl-mode . (lambda ()
                               (my-racket-mode-hook)
                               (company-mode))))
  :config
  (defun my-racket-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-dabbrev-code company-files company-yasnippet)))))

(provide 'setup-racket)
