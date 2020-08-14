;;; setup-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-14 13:03:07 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; golang configuration

;; go-mode: major-mode for editing go files
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :hook ((go-mode . (lambda ()
                      (lsp-deferred)
                      (lsp-ui-doc-mode)
                      (wh/set-go-tab-width)
                      (my-go-mode-hook)
                      (company-mode))))
  :config

  (defun my-go-mode-hook()
      (set (make-local-variable 'company-backends)
           '((company-capf company-files :with company-yasnippet)
             (company-dabbrev-code company-dabbrev))))

  ;; Go is indented with tabs, so set the tab size in those buffers.
  (defun wh/set-go-tab-width ()
    (setq-local indent-tabs-mode t)
    (setq tab-width 4))

  ;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; gotest: Emacs mode to go unit test command line tool
;; https://github.com/nlamirault/gotest.el
(use-package gotest
  :after go-mode)

(provide 'setup-go)

;; install gopls, godef
;; go get golang.org/x/tools/gopls
;; go get github.com/rogpeppe/godef
