;; Time-stamp: <2016-10-07 11:19:54 csraghunandan>

;; LaTeX
;; https://www.gnu.org/software/auctex/
(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn
    (require 'tex-mode)
    ;; (load "auctex")
    )
  :config
  ;; http://www.gnu.org/software/auctex/manual/auctex/Multifile.html
  (setq TeX-PDF-mode   t)
  (setq TeX-auto-save  t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq-default TeX-master nil))

(provide 'setup-tex)
