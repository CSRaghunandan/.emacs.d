;; Time-stamp: <2018-06-28 16:44:50 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; markdown: simple and quick outline mode
;; http://jblevins.org/projects/markdown-mode/
;; https://github.com/defunkt/markdown-moed
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-list-indent-width 2)

  ;; Don't change font in code blocks
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil))

;; markdown-toc: generate table of contents for markdown
;; https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :after markdown-mode)

(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)
