;;; setup-origami.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-04 22:05:00 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
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
    ("q" nil "Quit" :color blue))

  (global-origami-mode))

;; lsp-origami provides support for origami.el using language server protocol’s
;; textDocument/foldingRange functionality.
;; https://github.com/emacs-lsp/lsp-origami/
(use-package lsp-origami
  :hook ((lsp-after-open . lsp-origami-mode)))

(provide 'setup-origami)
