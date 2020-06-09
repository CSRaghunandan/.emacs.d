;;; setup-tree-sitter.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-09 23:43:14 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; run `tree-sitter-langs-install-grammars' to install the grammar files for
;;     languages for tree-sitter
;; Also, make sure `tree-sitter'is installed in your system
(use-package tree-sitter
  :straight (tree-sitter :type git
                         :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el" "src" "Cargo.toml" "Cargo.lock"))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(rustic-mode . rust)))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :hook ((rustic-mode . tree-sitter-mode)
         (rustic-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode))
  :after tree-sitter
  :init (require 'tree-sitter-langs))

(provide 'setup-tree-sitter)
