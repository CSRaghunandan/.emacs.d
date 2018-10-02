;;; setup-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-10-02 23:46:02 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; all the configuration for C/C++ projects

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
  :commands (lsp-css-enable)
  :init
;;;###autoload
  (defvar +ccls-path-mappings [])

;;;###autoload
  (defvar +ccls-initial-blacklist [])

  (setq ccls-executable (executable-find "ccls"))

  (setq
   ccls-extra-init-params
   `(:clang (:pathMappings ,+ccls-path-mappings)
            :completion
            (:includeBlacklist
             ("^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
              "^/usr/(local/)?include/c\\+\\+/v1/"
              ))
            :index (:initialBlacklist ,+ccls-initial-blacklist :trackDependency 1)))
  :config
  ;; enable ccls semantic highlighting
  (setq ccls-sem-highlight-method 'font-lock)

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  ;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el#L10
  (defun ccls/callee ()
    (interactive)
    (lsp-ui-peek-find-custom 'callee "$ccls/call" '(:callee t)))
  (defun ccls/caller ()
    (interactive)
    (lsp-ui-peek-find-custom 'caller "$ccls/call"))
  (defun ccls/vars (kind)
    (lsp-ui-peek-find-custom 'vars "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels)
    (lsp-ui-peek-find-custom 'base "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels)
    (lsp-ui-peek-find-custom 'derived "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind)
    (lsp-ui-peek-find-custom 'member "$ccls/member" `(:kind ,kind)))
  ;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

  ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
  (defun ccls/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom
     'address "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 128))))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro ()
    (interactive)
    (lsp-ui-peek-find-custom
     'address "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 64))))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom
     'address "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:excludeRole 32))))

  ;; References w/ Role::Read
  (defun ccls/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom
     'read "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 8))))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom
     'write "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 16)))))

(defun ccls//enable ()
  "Enable lsp-ccls"
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))

(use-package cc-mode :ensure nil
  :hook (((c++-mode c-mode) . (lambda ()
                                (ccls//enable)
                                (lsp-ui-mode)
                                (eldoc-mode)
                                (lsp-ui-sideline-mode)
                                (flycheck-mode)
                                (smart-dash-mode)
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
                 (c++-indent-level . 4)
                 (c-basic-offset . 4)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))
  (setq c-default-style "llvm")

  :config

  (defun my-cc-common-mode-hook()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'c++-mode-hook #'my-cc-common-mode-hook)
  (add-hook 'c-mode-hook #'my-cc-common-mode-hook)

  (add-hook 'c++-mode-hook (lambda ()
                             (setq-local company-transformers nil)
                             (setq-local company-lsp-async t)
                             (setq-local company-lsp-cache-candidates nil)))
  (add-hook 'c-mode-hook (lambda ()
                           (setq-local company-transformers nil)
                           (setq-local company-lsp-async t)
                           (setq-local company-lsp-cache-candidates nil)))

  ;;;###autoload
  (defun +cc|fontify-constants ()
    "Better fontification for preprocessor constants"
    (when (memq major-mode '(c-mode c++-mode))
      (font-lock-add-keywords
       nil '(("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
             ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
       t)))

  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  (defun +cc--re-search-for (regexp)
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (re-search-forward regexp magic-mode-regexp-match-limit t)))))

  (defun +cc-c-c++-objc-mode (&optional file)
    "Sets either `c-mode', `objc-mode' or `c++-mode', whichever is appropriate."
    (let ((base (file-name-sans-extension buffer-file-name))
          file)
      (cond ((file-exists-p! (or (concat base ".cpp")
                                 (concat base ".cc")))
             (c++-mode))
            ((or (file-exists-p! (or (concat base ".m")
                                     (concat base ".mm")))
                 (+cc--re-search-for
                  (concat "^[ \t\r]*\\(?:"
                          "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                          "\\|#import +<Foundation/Foundation.h>"
                          "\\|[-+] ([a-zA-Z0-9_]+)"
                          "\\)")))
             (objc-mode))
            ((fboundp 'c-or-c++-mode) ; introduced in Emacs 26.1
             (c-or-c++-mode))
            ((+cc--re-search-for  ; TODO Remove this along with Emacs 25 support
              (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
                (concat "^" ws-maybe "\\(?:"
                        "using"     ws "\\(?:namespace" ws "std;\\|std::\\)"
                        "\\|" "namespace" "\\(:?" ws id "\\)?" ws-maybe "{"
                        "\\|" "class"     ws id ws-maybe "[:{\n]"
                        "\\|" "template"  ws-maybe "<.*>"
                        "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                        "\\)")))
             (c++-mode))
            ((c-mode)))))

  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/cc/
  ;; Activate `c-mode', `c++-mode' or `objc-mode' depending on heuristics
  (add-to-list 'auto-mode-alist '("\\.h\\'" . +cc-c-c++-objc-mode)))

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
