;; Time-stamp: <2017-03-09 22:10:12 csraghunandan>

;; region-bindings-mode: enables custom bindings when mark is active
;; https://github.com/fgallina/region-bindings-mode
(use-package region-bindings-mode
  :diminish (region-bindings-mode . "")
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
   ("q" . vr/query-replace)
   (";" . comment-or-uncomment-region)
   ("s" . rag/kill-rectangle-replace-with-space)
   ("l" . align-hydra/body)
   ("t" . xah-title-case-region-or-line)))

(provide 'setup-region-bindings-mode)
