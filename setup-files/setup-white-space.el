;; Time-stamp: <2017-02-08 12:19:30 csraghunandan>

;; ws-butler: clean trailing whitespaces unobtrusively
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :diminish ws-butler-mode "𝐰"
  :config (add-hook 'prog-mode-hook 'ws-butler-mode))

(provide 'setup-white-space)
