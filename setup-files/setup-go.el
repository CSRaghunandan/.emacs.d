;;; setup-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:57:24 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; golang configuration

;; go-mode: major-mode for editing go files
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :interpreter "go"
  :ensure-system-package (goimports . "go get -u golang.org/x/tools/cmd/goimports")
  :config
  (setq gofmt-command (executable-find "goimports"))
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

  (add-hook 'go-mode-hook #'flycheck-mode)

  ;; integrate go-guru analysis tool to emacs
  (use-package go-guru
    :ensure-system-package (guru . "go get -u golang.org/x/tools/cmd/guru"))

  ;; gorepl-mode: A minor emacs mode for Go REPL.
  ;; https://github.com/manute/gorepl-mode
  (use-package gorepl-mode
    :ensure-system-package (gore . "go get -u github.com/motemen/gore")
    :commands (gorepl-run gorepl-run-load-current-file))

  ;; company-go: company backend for golang
  ;; https://github.com/nsf/gocode/tree/master/emacs-company
  (use-package company-go
    :ensure-system-package (gocode . "go get -u github.com/nsf/gocode")
    :config
    (defun my-go-mode-hook()
      (set (make-local-variable 'company-backends)
           '((company-go company-files :with company-yasnippet)
             (company-dabbrev-code company-dabbrev))))

    (add-hook 'go-mode-hook (lambda ()
                              (company-mode)
                              (my-go-mode-hook))))

  ;; gotest: Emacs mode to go unit test command line tool
  ;; https://github.com/nlamirault/gotest.el
  (use-package gotest)

  ;; go-rename: extra refactoring commands for go
  (use-package go-rename
    :ensure-system-package
    (gorename . "go get -u golang.org/x/tools/cmd/gorename")))

(provide 'setup-go)

;; commands to install all the tools required for go configuration to work
;; go get -u github.com/motemen/gore
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/guru
