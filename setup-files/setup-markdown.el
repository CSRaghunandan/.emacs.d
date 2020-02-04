;;; setup-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-04 17:24:39 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; markdown: simple and quick outline mode
;; http://jblevins.org/projects/markdown-mode/
;; https://github.com/defunkt/markdown-moed
(use-package markdown-mode
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :hook ((markdown-mode . (lambda ()
                            (setq-local display-line-numbers-type 'absolute))))
  :init
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments"))
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-list-indent-width 2
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t) ; for compat with org-mode
  ;; Don't change font in code blocks
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil))

;; markdown-toc: generate table of contents for markdown
;; https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :after markdown-mode)

;; pandoc-mode: An Emacs minor mode for interacting with Pandoc
;; https://github.com/joostkremers/pandoc-mode
(use-package pandoc-mode
  :commands pandoc-mode
  :hook (markdown-mode . conditionally-turn-on-pandoc))

(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)
