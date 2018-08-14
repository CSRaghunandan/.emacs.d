;;; setup-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:54:13 csraghunandan>

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

;; clang-format: format C/C++ buffers using clang-format
;; https://github.com/emacsorphanage/clang-format
(use-package clang-format)

;; ccls: Emacs client for ccls, a C/C++ language server
;; https://github.com/MaskRay/emacs-ccls
(use-package ccls
  :commands (lsp-css-enable)
  :init
  (setq ccls-executable (executable-find "ccls"))
  (setq ccls-extra-init-params '(:cacheFormat "msgpack"
                                 :index (:comments 2)
                                 :completion (:detailedLabel t)))
  :config
  ;; enable ccls semantic highlighting
  (setq ccls-sem-highlight-method 'font-lock))

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
                                (flycheck-mode)
                                (smart-dash-mode)
                                (company-mode)))
         ((c++-mode c-mode) . (lambda ()
                                (setq-local company-transformers nil)
                                (setq-local company-lsp-async t)
                                (setq-local company-lsp-cache-candidates nil)))
         ((c-mode c++-mode) . (lambda ()
                                (add-hook 'before-save-hook
                                          (lambda ()
                                            (time-stamp)
                                            (clang-format-buffer)) nil t))))
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
