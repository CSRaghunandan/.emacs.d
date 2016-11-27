;; Time-stamp: <2016-11-27 19:14:19 csraghunandan>

;; js2-mode, tern, company-tern, js2-refactor

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  ("\\.js$" . js2-mode)
  ("\\.json$" . js2-jsx-mode)
  :config
  ;; tern :- IDE like features for javascript and completion
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package tern
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (add-hook 'js2-mode-hook 'company-mode))

  (add-hook 'js2-mode-hook 'tern-mode)

  ;; company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern)

  ;; js2-refactor :- refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor :defer t
    :diminish js2-refactor
    :bind ("C-c j r" . js2r-add-keybindings-with-prefix))

  ;; Run a JavaScript interpreter in an inferior process window
  ;; https://github.com/redguardtoo/js-comint
  (use-package js-comint
    :bind (:map js2-mode-map
                (("C-x C-e" . js-send-last-sexp)
                 ("C-M-x" . js-send-last-sexp-and-go)
                 ("C-c C-b" . js-send-buffer-and-go)
                 X("C-c C-l" . js-load-file-and-go)))
    :config
    (setq inferior-js-program-command "node")))

(provide 'setup-js)
