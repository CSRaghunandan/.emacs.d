;; Time-stamp: <2017-02-14 18:02:27 csraghunandan>

;; JavaScript configuration

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode)
   ("\\.json$" . js2-jsx-mode)
   ("\\.jsx$" . js2-jsx-mode))
  :config
  ;; extra features for imenu
  (js2-imenu-extras-setup)

  ;; tern: IDE like features for javascript and completion
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
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)

  ;; enable flycheck in js2-mode
  (add-hook 'js2-mode-hook 'flycheck-mode)

  ;; company-tern: company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern)

  ;; js2-refactor: refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor :defer t
    :diminish js2-refactor-mode "𝐉𝐫"
    :config (js2r-add-keybindings-with-prefix "C-c j r"))

  (add-hook 'js2-mode-hook 'js2-refactor-mode)

  ;; web-beautify: Format HTML, CSS and JavaScript/JSON by js-beautify
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
                              (web-beautify-js-buffer)) nil t)))))

  ;; json-snatcher: get the path of any JSON element easily
  ;; https://github.com/Sterlingg/json-snatcher
  (use-package json-snatcher
    :config
    (defun js-mode-bindings ()
      "Sets a hotkey for using the json-snatcher plugin"
      (when (string-match  "\\.json$" (buffer-name))
        (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
    (add-hook 'js2-mode-hook 'js-mode-bindings))

  ;; mocha: emacs mode for running mocha tests
  ;; https://github.com/scottaj/mocha.el
  (use-package mocha
    :bind
    (("C-c m P" . mocha-test-project)
     ("C-c m f" . mocha-test-file)
     ("C-c m p" . mocha-test-at-point))))

(provide 'setup-js)
