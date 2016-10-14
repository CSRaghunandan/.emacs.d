;; Time-stamp: <2016-10-13 17:54:01 csraghunandan>

;; js2-mode, tern, company-tern, js2-refactor

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook 'electric-operator-mode)

  ;; tern :- IDE like features for javascript and completion
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package tern
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files company-yasnippet))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (add-hook 'js2-mode-hook 'company-mode))

  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'ggtags-mode)
  ;; company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern)

  ;; js2-refactor :- refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor :defer t
    :diminish js2-refactor
    :bind ("C-c j r" . js2r-add-keybindings-with-prefix)))

(provide 'setup-js)
