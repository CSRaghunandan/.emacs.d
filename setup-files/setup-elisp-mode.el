;; Time-stamp: <2016-10-06 22:18:58 csraghunandan>

;; emacs-lisp-mpde
;; configure company mode for emacs-lisp-mode
(use-package elisp-mode :ensure nil
  :config
  (defun my-elisp-mode-hook ()
    "Hook for `emacs-lisp-mode'"
    (set (make-local-variable 'company-backends)
         '((company-capf company-dabbrev-code company-yasnippet company-files))))
  (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(provide 'setup-elisp-mode)
