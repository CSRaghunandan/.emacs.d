;;; setup-tree-sitter.el -*- lexical-binding: t; -*-
;; Time-stamp: <2021-03-24 04:28:06 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; run `tree-sitter-langs-install-grammars' to install the grammar files for
;;     languages for tree-sitter
;; Also, make sure `tree-sitter'is installed in your system
(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :hook (((rustic-mode
           python-mode
           go-mode
           typescript-mode
           css-mode) . tree-sitter-mode)
         ((rustic-mode
           python-mode
           go-mode
           typescript-mode
           css-mode) . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :after tree-sitter)

(provide 'setup-tree-sitter)
