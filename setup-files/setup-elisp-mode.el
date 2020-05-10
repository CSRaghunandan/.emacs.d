;;; setup-elisp-mode.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-10 10:46:19 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; emacs-lisp-mpde
;; configure company mode for emacs-lisp-mode
(use-package elisp-mode :straight nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-l" . eval-buffer))
  :hook ((emacs-lisp-mode . my-elisp-mode-hook)
         (emacs-lisp-mode . company-mode))
  :config
  (defun my-elisp-mode-hook ()
    "Hook for `emacs-lisp-mode'"
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

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

  (add-hook 'after-save-hook #'byte-compile-current-buffer))

;; highlight-quoted: highlight lisp quoted and quotes symbols
;; https://github.com/Fanael/highlight-quoted
(use-package highlight-quoted
  :hook ((emacs-lisp-mode lisp-mode) . highlight-quoted-mode)
  :config
  ;; Highlight the ' character itself in the same colour
  ;; as the quoted symbol.
  (set-face-attribute 'highlight-quoted-quote nil
                      :inherit 'highlight-quoted-symbol))

;; Go to the definition of the symbol at point. Supports global definitions,
;; local definitions, and even macro-heavy code!
;; https://github.com/Wilfred/elisp-def
(use-package elisp-def
  :bind (:map emacs-lisp-mode-map
              ("M-." . elisp-def)
              ("M-," . xref-pop-marker-stack)))

(provide 'setup-elisp-mode)
