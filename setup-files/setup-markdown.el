;; Time-stamp: <2017-02-12 16:49:04 csraghunandan>

;; markdown: simple and quick outline mode
;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config

  ;; markdown-toc: generate table of contents for markdown
  ;; https://github.com/ardumont/markdown-toc
  (use-package markdown-toc))

(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)
