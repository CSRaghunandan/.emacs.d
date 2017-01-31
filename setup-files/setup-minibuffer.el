;; Time-stamp: <2017-01-31 10:30:41 csraghunandan>

;; configuration for minibuffer

;; recursive minibuffers
(setq enable-recursive-minibuffers t)   ; enable to use minibuffer recursively.
(if (booleanp enable-recursive-minibuffers)
    (minibuffer-depth-indicate-mode t))

;; resize minibuffer window to accommodate text
(setq resize-mini-window t)

(defun my-minibuffer-setup-hook ()
  "disable whole-line-or-region inside minibuffer"
  (whole-line-or-region-mode))
(defun my-minibuffer-exit-hook ()
  "enable it again after exiting minibuffer"
  (whole-line-or-region-mode))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; enable some minor modes when in eval-expression of minibuffer
(add-hook 'eval-expression-minibuffer-setup-hook (lambda()
                                                   (smartparens-mode)
                                                   (rainbow-delimiters-mode)
                                                   (eldoc-mode)))

(provide 'setup-minibuffer)
