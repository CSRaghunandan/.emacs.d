;; Time-stamp: <2017-06-20 22:47:30 csraghunandan>

;; markdown: simple and quick outline mode
;; http://jblevins.org/projects/markdown-mode/
;; https://github.com/defunkt/markdown-moed
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; markdown-toc: generate table of contents for markdown
  ;; https://github.com/ardumont/markdown-toc
  (use-package markdown-toc))

(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)
