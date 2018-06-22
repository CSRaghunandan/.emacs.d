;; Time-stamp: <2018-06-22 12:17:24 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; origami: better folding package than hs-minor-mode
;; https://github.com/gregsexton/origami.el
(use-package origami
  :config
  (global-origami-mode)

  (bind-key "C-c h o"
            (defhydra hydra-origami (:color red
                                            :hint nil)
"
_t_: toggle    _r_: redo    _p_: prev        _c_: close all
_u_: undo      _n_: next    _o_: open all    _q_: quit
"
              ("t" origami-recursively-toggle-node)
              ("u" origami-undo)
              ("r" origami-redo)
              ("p" origami-previous-fold)
              ("n" origami-next-fold)
              ("o" origami-open-all-nodes)
              ("c" origami-close-all-nodes)
              ("q" nil :color blue))))

(provide 'setup-origami)
