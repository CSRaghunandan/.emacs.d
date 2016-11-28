;; Time-stamp: <2016-11-28 16:17:38 csraghunandan>

;; js2-mode, tern, company-tern, js2-refactor

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode)
   ("\\.json$" . js2-jsx-mode))
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

  ;; provides REPL and inspect, debug tools by connecting to a chrom(e|ium) process
  ;; https://github.com/NicolasPetton/jade
  (use-package jade
    :bind (:map js2-mode-map
                ("C-c C-l" . jade-eval-buffer))
    :config
    (require 'seq-25)
    (add-hook 'js2-mode-hook #'jade-interaction-mode)))

(provide 'setup-js)

;; Jade
;; evaluate `jade-scratch' to get a scratch JS buffer
;; C-c C-l will evaluate the buffer
;; C-x C-e will evaluate the expression at point
;; C-c M-i will inspect the result
