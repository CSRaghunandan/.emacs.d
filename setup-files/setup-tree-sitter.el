;;; setup-tree-sitter.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-10-23 19:53:57 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; dynamic module needed for tree-sitter to work
(use-package tsc
  :straight (tsc :host github
                 :repo "ubolonton/emacs-tree-sitter"
                 :files ("core/*.el")))

;; run `tree-sitter-langs-install-grammars' to install the grammar files for
;;     languages for tree-sitter
;; Also, make sure `tree-sitter'is installed in your system
(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :straight (tree-sitter :type git
                         :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el"))
  :hook (((rustic-mode
           python-mode
           go-mode
           typescript-mode
           css-mode) . tree-sitter-mode)
         ((rustic-mode
           python-mode
           go-mode
           typescript-mode
           css-mode) . tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(rustic-mode . rust)))

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

(provide 'setup-tree-sitter)
