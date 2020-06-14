;;; setup-tex.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-14 19:28:21 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; LaTeX configuration using auctex
;; https://www.gnu.org/software/auctex/
(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind
  (:map LaTeX-mode-map
        ("C-c C-g" . reftex-grep-document))
  :config
  ;; http://www.gnu.org/software/auctex/manual/auctex/Multifile.html
  (setq TeX-PDF-mode   t)
  (setq TeX-auto-save  t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil) ; don't prompt when saving a tex file

    ;; Font-lock for AuCTeX
    ;; Note: '«' and '»' is by pressing 'C-x 8 <' and 'C-x 8 >', respectively
  (font-lock-add-keywords 'latex-mode
                          (list
                           (list "\\(«\\(.+?\\|\n\\)\\)\\(+?\\)\\(»\\)"
                                 '(1 'font-latex-string-face t)
                                 '(2 'font-latex-string-face t)
                                 '(3 'font-latex-string-face t))))
    ;; Add standard Sweave file extensions to the list of files recognized  by AuCTeX.
    (add-hook 'TeX-mode-hook (lambda () (reftex-isearch-minor-mode))))

(use-package reftex
  :commands turn-on-reftex
  :config (setq reftex-plug-into-AUCTeX t))

(provide 'setup-tex)
