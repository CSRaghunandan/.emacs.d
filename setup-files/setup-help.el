;; Time-stamp: <2017-06-12 12:04:44 csraghunandan>

;; helpful: A better Emacs *help* buffer
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  (("C-c h c" . helpful-command)
   ("C-c h f" . helpful-function)
   ("C-c h m" . helpful-macro)))

(provide 'setup-help)
