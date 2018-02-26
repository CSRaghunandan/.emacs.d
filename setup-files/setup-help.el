;; Time-stamp: <2018-02-26 11:57:24 csraghunandan>

;; helpful: A better Emacs *help* buffer
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind*
  (("C-? k" . helpful-key)
   ("C-? v" . helpful-variable)
   ("C-? f" . helpful-callable)
   ("C-? F" . helpful-function)
   ("C-? C" . helpful-command)
   ("C-c C-." . helpful-at-point)))

;; more info in apropos
(setq apropos-do-all t)
;; always select help window when opened
(setq-default help-window-select t)

(provide 'setup-help)
