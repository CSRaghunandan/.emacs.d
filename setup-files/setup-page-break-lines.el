;; Time-stamp: <2016-10-12 20:28:38 csraghunandan>

;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines

(use-package page-break-lines :defer 2
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(provide 'setup-page-break-lines)

;; to insert a page break character in emacs, type `C-q C-l'
