;; Time-stamp: <2017-10-27 16:46:47 csraghunandan>

;; selected: Keymap for when region is active
;; https://github.com/Kungsgeten/selected.el
(use-package selected
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("w" . kill-region)
              ("~" . hydra-change-case/body)
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
              ("t" . xah-title-case-region-or-line))
  :config
  (selected-global-mode))

(provide 'setup-selected)
