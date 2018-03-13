;; Time-stamp: <2018-03-13 10:18:29 csraghunandan>

;; ws-butler: clean trailing whitespaces unobtrusively
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)))

(provide 'setup-white-space)
