;; Time-stamp: <2016-10-07 12:36:29 csraghunandan>

;; markdown  :- simple and quick outline mode
;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn
    ;; http://daringfireball.net/projects/markdown/
    ;; Download the Markdown source from above, extract the .pl from that
    ;; and place it in one of the folders in the environment PATH
    (when (executable-find "Markdown.pl")
      (setq markdown-command "Markdown.pl"))

    ;; https://github.com/cadadr/emacs.d
    (defun gk-markdown-preview-buffer ()
      (interactive)
      (require 'shr)
      (let* ((buf-this (buffer-name (current-buffer)))
             (buf-html (get-buffer-create
                        (format "*md-html (%s)*" buf-this))))
        (markdown-other-window (buffer-name buf-html))
        (shr-render-buffer buf-html)
        (eww-mode)
        (kill-buffer buf-html)))

    (bind-keys
     :map markdown-mode-map
     ;; Mimicking the org-export style bindings
     ("C-c C-e o" . gk-markdown-preview-buffer))))

(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)
