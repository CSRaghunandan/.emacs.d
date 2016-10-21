;; Time-stamp: <2016-10-22 00:19:40 csraghunandan>

;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'org-mode-hook 'page-break-lines-mode))

(provide 'setup-page-break-lines)

;; to insert a page break character in emacs, type `C-q C-l'
