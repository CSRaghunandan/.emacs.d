;; Time-stamp: <2016-12-30 17:50:20 csraghunandan>

;; js2-mode, tern, company-tern, js2-refactor

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . jade-eval-buffer))
  :mode
  (("\\.js$" . js2-mode)
   ("\\.json$" . js2-jsx-mode)
   ("\\.jsx$" . js2-jsx-mode))
  :config
  ;; extra features for imenu
  (js2-imenu-extras-setup)

  ;; reduce the delay before re-parsing after user makes changes
  (add-hook 'js2-mode-hook (lambda ()
                             (setq-local js2-idle-timer-delay 0.01)))

  ;; tern :- IDE like features for javascript and completion
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package tern
    :diminish tern-mode "𝐓𝐞"
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files company-yasnippet))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (add-hook 'js2-mode-hook 'company-mode))

  (add-hook 'js2-mode-hook 'tern-mode)

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  ;; enable flycheck in js2-mode
  (add-hook 'js2-mode-hook 'flycheck-mode)

  ;; company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern)

  ;; js2-refactor :- refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor :defer t
    :diminish js2-refactor-mode "𝐉𝐫"
    :config
    (js2r-add-keybindings-with-prefix "C-c j r"))

  (add-hook 'js2-mode-hook 'js2-refactor-mode)

  ;; provides REPL and inspect, debug tools by connecting to a chrom(e|ium) process
  ;; https://github.com/NicolasPetton/jade
  (use-package jade
    :diminish (jade-interaction-mode . "𝐉𝐚")
    :init
    (add-hook 'js2-mode-hook #'jade-interaction-mode))

  ;; web-beautify - Format HTML, CSS and JavaScript/JSON by js-beautify
  ;; https://github.com/yasuyk/web-beautify
  (when (executable-find "js-beautify")
    (use-package web-beautify
      :config
      ;; format JS buffers on save
      (add-hook 'js2-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook
                            (lambda ()
                              (time-stamp)
                              (web-beautify-js-buffer)) nil t))))))

(provide 'setup-js)

;; Jade
;; evaluate `jade-scratch' when in JS REPL to get a scratch JS buffer
;; C-c C-l will evaluate the buffer
;; C-x C-e will evaluate the expression at point
;; C-c M-i will inspect the result
