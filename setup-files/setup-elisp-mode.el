;; Time-stamp: <2018-02-27 00:41:16 csraghunandan>

;; emacs-lisp-mpde
;; configure company mode for emacs-lisp-mode
(use-package elisp-mode :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-l" . eval-buffer))
  :config
  (defun my-elisp-mode-hook ()
    "Hook for `emacs-lisp-mode'"
    (set (make-local-variable 'company-backends)
         '((company-capf company-yasnippet company-files))))
  (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)

  (defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (time-stamp)
                          (xah-clean-whitespace)) nil t)))

  (add-hook 'after-save-hook #'byte-compile-current-buffer)

  ;; highlight-quoted: highlight lisp quoted and quotes symbols
  ;; https://github.com/Fanael/highlight-quoted
  (use-package highlight-quoted
    :config
    (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
    (add-hook 'lisp-mode-hook #'highlight-quoted-mode)

    ;; Highlight the ' character itself in the same colour
    ;; as the quoted symbol.
    (set-face-attribute 'highlight-quoted-quote nil
                        :inherit 'highlight-quoted-symbol)))

(provide 'setup-elisp-mode)
