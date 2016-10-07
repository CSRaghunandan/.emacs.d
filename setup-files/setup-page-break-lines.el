;; Time-stamp: <2016-10-07 19:21:24 csraghunandan>

;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines

(use-package page-break-lines :defer 2
  :config
  (add-hook 'prog-mode-hook (lambda() (turn-on-page-break-lines-mode))))

(provide 'setup-page-break-lines)
