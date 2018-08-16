;;; setup-racket.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-16 15:44:17 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; racket-mode: major mode for editing racket files
;; https://github.com/greghendershott/racket-mode
(use-package racket-mode :defer t
  :hook ((racket-mode . (lambda ()
                          (my-racket-mode-hook)
                          (company-mode)))
         (racket-mode . highlight-quoted-mode)
         (racket-repl-mode . (lambda ()
                               (my-racket-mode-hook)
                               (company-mode))))
  :bind (:map racket-mode-map
              ("C-c C-t" . racket-describe))
  :config
  (defun my-racket-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))))

(provide 'setup-racket)
