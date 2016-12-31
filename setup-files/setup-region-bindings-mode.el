;; Time-stamp: <2016-12-31 13:20:00 csraghunandan>

;; Region Bindings Mode
;; https://github.com/fgallina/region-bindings-mode

;; Minor mode that enables the ability of having a custom keys for working with
;; regions. This is a pretty good way to keep the global bindings clean.

(use-package region-bindings-mode
  :diminish (region-bindings-mode . "𝐑𝐞")
  :config
  (region-bindings-mode-enable)

  (bind-keys
   :map region-bindings-mode-map
   ("w" . kill-region)
   ("c" . copy-region-as-kill)
   ("d" . duplicate-current-line-or-region)
   ("N" . rag/narrow-or-widen-dwim)
   ("E" . eval-region)
   ("e" . er/expand-region)
   ("f" . fill-region)
   ("q" . query-replace)
   (";" . comment-or-uncomment-region)))

(provide 'setup-region-bindings-mode)
