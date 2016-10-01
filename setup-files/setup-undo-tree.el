(use-package undo-tree
  :diminish undo-tree-mode
  :bind* (("C-c u" . undo-tree-undo)
          ("C-c r" . undo-tree-redo)
          ("C-c U" . undo-tree-visualize))
  :config (global-undo-tree-mode))

(provide 'setup-undo-tree)
