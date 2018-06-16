;; Timestamp: <2017-07-22 13:53:13>
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

;; cquery: Emacs client for cquery, a low-latency language server supporting multi-million line C++ code-bases
;; https://github.com/cquery-project/emacs-cquery
(use-package cquery
  :init
  (setq cquery-executable (executable-find "cquery"))
  (setq cquery-extra-init-params
        '(:index (:comments 2) :cacheFormat "msgpack"
                 :completion (:detailedLabel t)))
  :config
  ;; enable cquery semantic highlighting
  (setq cquery-sem-highlight-method 'font-lock))

(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(use-package cc-mode :ensure nil
  :hook (((c++-mode c-mode) . (lambda ()
                                (cquery//enable)
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
                                            (lsp-format-buffer)) nil t))))
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

  (c-add-style "llvm"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 4)
                 (c-basic-offset . 4)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))
  (setq-default c-default-style "llvm"))

(provide 'setup-cc)

;; cquery cross-reference extension:
;; (lsp-ui-peek-find-custom 'base "$cquery/base")
;; (lsp-ui-peek-find-custom 'callers "$cquery/callers")
;; (lsp-ui-peek-find-custom 'derived "$cquery/derived")
;; (lsp-ui-peek-find-custom 'vars "$cquery/vars")

;; Call/member/inheritance Hierarchies
;; cquery-member-hierarchy
;; (cquery-call-hierarchy nil) -> caller hierarchy
;; (cquery-call-hierarchy t) -> callee hierarchy
;; (cquery-inheritance-hierarchy nil) -> base hierarchy
;; (cquery-inheritance-hierarchy t) -> derived hierarchy
