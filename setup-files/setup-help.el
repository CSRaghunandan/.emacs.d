;; Time-stamp: <2018-03-02 15:57:42 csraghunandan>

;; helpful: A better Emacs *help* buffer
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind*
  (([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-function)
   ("C-? k" . helpful-key)
   ("C-? f" . helpful-callable)
   ("C-? C" . helpful-command)
   ("C-c C-." . helpful-at-point)))

;; more info in apropos
(setq apropos-do-all t)
;; always select help window when opened
(setq-default help-window-select t)

(provide 'setup-help)
