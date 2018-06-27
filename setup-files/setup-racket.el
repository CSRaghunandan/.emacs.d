;; Time-stamp: <2018-06-27 18:59:23 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

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
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))))

(provide 'setup-racket)
