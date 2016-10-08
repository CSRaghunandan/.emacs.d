;; Time-stamp: <2016-10-08 08:43:19 csraghunandan>

;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines

(use-package page-break-lines :defer 2
  :diminish page-break-lines-mode
  :config
  (add-hook 'prog-mode-hook (lambda() (turn-on-page-break-lines-mode))))

(provide 'setup-page-break-lines)

;; to insert a page break character in emacs, type `C-q C-l'
