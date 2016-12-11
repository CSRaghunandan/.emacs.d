;; Time-stamp: <2016-12-11 13:08:15 csraghunandan>

;; Region Bindings Mode
;; https://github.com/fgallina/region-bindings-mode

;; Minor mode that enables the ability of having a custom keys for working with
;; regions. This is a pretty good way to keep the global bindings clean.

(use-package region-bindings-mode
  :diminish region-bindings-mode
  :config
  (region-bindings-mode-enable)

  (bind-keys
   :map region-bindings-mode-map
   ("w" . kill-region)
   ("c" . copy-region-as-kill)
   ("(" . wrap-wth-parens)
   ("{" . wrap-with-braces)
   ("'" . wrap-with-single-quotes)
   ("\"" . wrap-with-double-quotes)
   ("d" . duplicate-current-line-or-region)
   ("N" . rag/narrow-or-widen-dwim)
   ("E" . eval-region)
   ("e" . er/expand-region)
   ("f" . fill-region)
   ("q" . query-replace)))

(provide 'setup-region-bindings-mode)
