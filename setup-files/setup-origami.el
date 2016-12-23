;; Time-stamp: <2016-12-23 16:35:56 csraghunandan>

;; origami - provides folding capabilities
;; https://github.com/gregsexton/origami.el
(use-package origami
  :config
  (global-origami-mode)

  (bind-key "C-c h o"
            (defhydra hydra-origami (:color red)
              ("t" origami-recursively-toggle-node "toggle")
              ("u" origami-undo "undo")
              ("r" origami-redo "redo")
              ("x" origami-reset "redo")
              ("p" origami-previous-fold "prev")
              ("n" origami-next-fold "next")
              ("o" origami-open-all-nodes "open all")
              ("c" origami-close-all-nodes "close all"))))

(provide 'setup-origami)
