;; Time-stamp: <2016-10-09 22:16:04 csraghunandan>

;; Treat undo history as a tree
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("s-/" . undo-tree-redo))
  :init (global-undo-tree-mode))

(provide 'setup-undo-tree)
