;; Time-stamp: <2017-09-04 16:03:55 csraghunandan>

;; golang configuration

;; go-mode: major-mode for editing go files
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :interpreter "go"
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
    :commands go-eldoc-setup
    :config (add-hook 'go-mode-hook 'go-eldoc-setup))

  ;; go-playground:GNU/Emacs mode that setup local Go playground for code
  ;; snippets like play.golang.org or even better :)
  ;; https://github.com/grafov/go-playground
  (use-package go-playground
    :bind (:map go-mode-map
                (("C-c g p" . go-playground))))

  (add-hook 'go-mode-hook #'flycheck-mode)

  ;; integrate go-guru analysis tool to emacs
  (use-package go-guru
    :if (executable-find "guru")
    :config
    (unless (executable-find "guru")
      (warn "go-mode: couldn't find guru, refactoring commands won't work")))

  ;; gorepl-mode: A minor emacs mode for Go REPL.
  ;; https://github.com/manute/gorepl-mode
  (use-package gorepl-mode
    :if (executable-find "gore")
    :commands (gorepl-run gorepl-run-load-current-file)
    :config
    (unless (executable-find "gore")
      (warn "go-mode: couldn't find gore, REPL support disabled")))

  ;; company-go: company backend for golang
  ;; https://github.com/nsf/gocode/tree/master/emacs-company
  (use-package company-go
    :if (executable-find "gocode")
    :init (setq command-go-gocode-command "gocode")
    :config

    (defun my-go-mode-hook()
      (set (make-local-variable 'company-backends)
           '((company-go company-files company-yasnippet))))

    (if (executable-find command-go-gocode-command)
        (add-hook 'go-mode-hook (lambda ()
                                  (company-mode)
                                  (my-go-mode-hook)))
      (warn "go-mode: couldn't find gocode, code completion won't work")))

  ;; gotest: Emacs mode to go unit test command line tool
  ;; https://github.com/nlamirault/gotest.el
  (use-package gotest)

  ;; go-rename: extra refactoring commands for go
  (use-package go-rename
    :if (executable-find "gorename")
    :config
    (unless (executable-find "gorename")
      (warn "go-mode: couldn't find gorename, extra refactoring commands won't work"))))

(provide 'setup-go)

;; commands to install all the tools required for go configuration to work
;; go get -u github.com/motemen/gore
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/guru
