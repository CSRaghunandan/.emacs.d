;; Time-stamp: <2017-06-15 14:22:12 csraghunandan>

;; golang configuration

;; go-mode: major-mode for editing go files
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :config
  (setq gofmt-command "~/go/bin/goimports")
  ;; Using -s with goimports is not supported with upstream goimports.
  ;; See https://github.com/golang/go/issues/8759 . Instead, use
  ;; $ go get github.com/jzelinskie/tools/cmd/goimports
  (setq gofmt-args (list "-s"))

  (defun wh/gofmt-before-save ()
    (set (make-local-variable 'before-save-hook)
         (append before-save-hook (list #'gofmt-before-save))))

  (add-hook 'go-mode-hook #'wh/gofmt-before-save)

  ;; Go is indented with tabs, so set the tab size in those buffers.
  (defun wh/set-go-tab-width ()
    (setq-local indent-tabs-mode t)
    (setq tab-width 4))

  (add-hook 'go-mode-hook #'wh/set-go-tab-width)

  ;; go-eldoc: eldoc for go language
  ;; https://github.com/syohex/emacs-go-eldoc
  (use-package go-eldoc
    :config (add-hook 'go-mode-hook 'go-eldoc-setup))

  ;; go-playground:GNU/Emacs mode that setup local Go playground for code
  ;; snippets like play.golang.org or even better :)
  ;; https://github.com/grafov/go-playground
  (use-package go-playground
    :bind (:map go-mode-map
                (("C-c g p" . go-playground))))

  (add-hook 'go-mode-hook #'flycheck-mode)

  ;;TODO: company setup for go
  ;;TODO: go to def setup
  ;;TODO: refactoring tools setup
  )

(provide 'setup-go)
