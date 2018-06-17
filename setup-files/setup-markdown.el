;; Time-stamp: <2018-06-17 17:55:24 csraghunandan>

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
  (use-package markdown-toc)

  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-list-indent-width 2)
  (setq-default markdown-hide-markup t)

  (set-face-attribute markdown-header-face-1 nil :foreground "#DFAF8F" :weight 'bold)
  (set-face-attribute markdown-header-face-2 nil :foreground "#BFEBBF" :weight 'bold)
  (set-face-attribute markdown-header-face-3 nil :foreground "#7CB8BB" :weight 'bold)
  (set-face-attribute markdown-header-face-4 nil :foreground "#D0BF8F" :weight 'bold)
  (set-face-attribute markdown-header-face-5 nil :foreground "#93E0E3" :weight 'bold)
  (set-face-attribute markdown-header-face-6 nil :foreground "#9FC59F" :weight 'bold))

(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)
