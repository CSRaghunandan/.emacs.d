;; Time-stamp: <2016-10-06 22:27:28 csraghunandan>

;; Treat undo history as a tree
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind* (("C-c u" . undo-tree-undo)
          ("C-c r" . undo-tree-redo)
          ("C-c U" . undo-tree-visualize))
  :config (global-undo-tree-mode))

(provide 'setup-undo-tree)
