;; Time-stamp: <2017-02-07 11:19:23 csraghunandan>

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
   ("W" . recursive-narrow-or-widen-dwim)
   ("E" . eval-region)
   ("e" . er/expand-region)
   ("f" . fill-region)
   ("q" . query-replace)
   (";" . comment-or-uncomment-region)
   ("s" . rag/kill-rectangle-replace-with-space)
   ("l" . align-hydra/body)))

(provide 'setup-region-bindings-mode)
