;;; setup-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-13 14:44:15 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; all the configuration for C/C++ projects

;; cmake-mode: major mode for cmake files
;; https://gitlab.kitware.com/cmake/cmake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode)

;; cmake-font-lock: emacs font lock rules for CMake
;; https://github.com/Lindydancer/cmake-font-lock
(use-package cmake-font-lock
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

;; adds font-lock highlighting for modern C++ upto C++17
;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; ccls: Emacs client for ccls, a C/C++ language server
;; https://github.com/MaskRay/emacs-ccls
(use-package ccls
  :init
  (setq ccls-executable (executable-find "ccls"))

  :config
  ;; enable ccls semantic highlighting
  (setq ccls-sem-highlight-method 'font-lock)

  ;;;###autoload
  (defvar +ccls-path-mappings [])

;;;###autoload
  (defvar +ccls-initial-blacklist [])

  (setq
   ccls-initialization-options
   `(:clang
     (:excludeArgs
      ;; Linux's gcc options. See ccls/wiki
      ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
       "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
       "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
      :extraArgs []
      :pathMappings ,+ccls-path-mappings)
     :completion
     (:include
      (:blacklist
       ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
        "^/usr/(local/)?include/c\\+\\+/v1/"
        ]))
     :index (:initialBlacklist ,+ccls-initial-blacklist :parametersInDeclarations :json-false :trackDependency 1)))

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))

  ;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el#L10
  (defun ccls/callee ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind)
    (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind)
    (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

  ;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h
  ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
  (defun ccls/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 128)))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 64)))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :excludeRole 32)))

  ;; References w/ Role::Read
  (defun ccls/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8)))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16))))

(defun ccls//enable ()
  "Enable lsp-ccls"
  (require 'ccls)
  (lsp-deferred))

(use-package cc-mode :straight nil
  :hook (((c++-mode c-mode) . (lambda ()
                                (ccls//enable)
                                (setq-local ccls-code-lens-mode t)
                                (lsp-ui-doc-mode)
                                (+cc-fontify-constants-h)
                                (company-mode)))
         ((c-mode c++-mode) . (lambda ()
                                (add-hook 'before-save-hook
                                          (lambda ()
                                            (time-stamp)
                                            (lsp-format-buffer)) nil t))))
  :init
  (c-add-style "llvm"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))
  (setq c-default-style "llvm")

  :config

  (defun my-cc-common-mode-hook()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'c++-mode-hook #'my-cc-common-mode-hook)
  (add-hook 'c-mode-hook #'my-cc-common-mode-hook)

  ;;;###autoload
  (defun +cc-fontify-constants-h ()
    "Better fontification for preprocessor constants"
    (when (memq major-mode '(c-mode c++-mode))
      (font-lock-add-keywords
       nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
             ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
       t)))

  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

;; highlight doxygen comments in Emacs, including code blocks
;; https://github.com/Lindydancer/highlight-doxygen/tree/master
(use-package highlight-doxygen
  :hook ((c-mode c++-mode) . highlight-doxygen-mode))

;; Major mode for editing QT Declarative (QML) code.
;; https://github.com/coldnew/qml-mode
(use-package qml-mode
  :mode ("\\.qml$" . qml-mode))

(provide 'setup-cc)

;; (ccls-xref-find-custom "$ccls/base")
;; (ccls-xref-find-custom "$ccls/callers")
;; Use lsp-goto-implementation or lsp-ui-peek-find-implementation for derived types/functions
;; (ccls-xref-find-custom "$ccls/vars")

;; ;; Alternatively, use lsp-ui-peek interface
;; (lsp-ui-peek-find-custom 'base "$ccls/base")
;; (lsp-ui-peek-find-custom 'callers "$ccls/callers")
;; (lsp-ui-peek-find-custom 'random "$ccls/random") ;; jump to a random declaration

;; (ccls-member-hierarchy)
;; (ccls-call-hierarchy nil) ; caller hierarchy
;; (ccls-call-hierarchy t) ; callee hierarchy
;; (ccls-inheritance-hierarchy nil) ; base hierarchy
;; (ccls-inheritance-hierarchy t) ; derived hierarchy
