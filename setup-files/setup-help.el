;; Time-stamp: <2017-12-02 13:26:34 csraghunandan>

;; helpful: A better Emacs *help* buffer
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  (("C-c e c" . helpful-command)
   ("C-c e f" . helpful-function)
   ("C-c e m" . helpful-macro)))

;; more info in apropos
(setq apropos-do-all t)
;; always select help window when opened
(setq-default help-window-select t)

(provide 'setup-help)
