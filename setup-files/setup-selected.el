;;; setup-selected.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 03:07:48 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; selected: Keymap for when region is active
;; https://github.com/Kungsgeten/selected.el
(use-package selected
  :bind (:map selected-keymap
              ("w" . kill-region)
              ("~" . hydra-change-case/body)
              ("c" . copy-region-as-kill)
              ("d" . duplicate-current-line-or-region)
              ("E" . eval-region)
              ("e" . er/expand-region)
              ("f" . fill-region)
              ("q" . vr/query-replace)
              (";" . comment-or-uncomment-region)
              ("s" . rag/kill-rectangle-replace-with-space)
              ("l" . align-hydra/body)
              ("t" . xah-title-case-region-or-line))
  :init
  (selected-global-mode))

(provide 'setup-selected)
