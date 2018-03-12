;; Time-stamp: <2018-03-12 22:49:07 csraghunandan>

;; ws-butler: clean trailing whitespaces unobtrusively
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :config (add-hook 'prog-mode-hook 'ws-butler-mode))

(provide 'setup-white-space)
