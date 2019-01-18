;;; setup-origami.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-01-18 16:37:45 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; origami: better folding package than hs-minor-mode
;; https://github.com/gregsexton/origami.el
(use-package origami
  :bind ("C-c h o" . hydra-origami/body)
  :config
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
    ("q" nil :color blue))

  (global-origami-mode))

(provide 'setup-origami)
