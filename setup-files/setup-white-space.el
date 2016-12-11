;; Time-stamp: <2016-12-12 03:37:12 csraghunandan>

;; clean trailing whitespaces on the fly
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :diminish ws-butler-mode "𝐰"
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(provide 'setup-white-space)
