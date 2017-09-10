;; Time-stamp: <2017-09-10 08:58:53 csraghunandan>

;; helpful: A better Emacs *help* buffer
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  (("C-c e c" . helpful-command)
   ("C-c e f" . helpful-function)
   ("C-c e m" . helpful-macro)))

(provide 'setup-help)
