;; Time-stamp: <2017-02-12 16:54:53 csraghunandan>

;; Python configuration
(use-package python
  :bind (:map python-mode-map
              (("C-c C-t" . anaconda-mode-show-doc)
               ("M-." . anaconda-mode-find-definitions)
               ("M-," . anaconda-mode-go-back-definitions)))
  :config
  (setq python-shell-interpreter "python3")
  ;; don't try to guess python indent offset
  (setq python-indent-guess-indent-offset nil)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'smart-dash-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  ;; enable company-mode completions in inferior python process
  (add-hook 'inferior-python-mode-hook 'company-mode)

  ;; anaconda-mode: bring IDE like features for python-mode
  ;; https://github.com/proofit404/anaconda-mode
  (use-package anaconda-mode
    :diminish (anaconda-mode . "𝐀𝐧")
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

  ;; company-anaconda: company backend for anaconda
  ;; https://github.com/proofit404/company-anaconda
  (use-package company-anaconda
    :config
    (defun my-anaconda-mode-hook ()
      "Hook for `web-mode'."
      (set (make-local-variable 'company-backends)
           '((company-anaconda company-files company-yasnippet))))
    (add-hook 'python-mode-hook 'my-anaconda-mode-hook))

  ;; pytest: for testing python code
  ;; https://github.com/ionrock/pytest-el
  (use-package pytest :defer t)

  ;; yapfify: format python files automatically
  ;; https://github.com/JorisE/yapfify
  (use-package yapfify
    :diminish (yapf-mode . "𝐘𝐚")
    :config
    (add-hook 'python-mode-hook 'yapf-mode))

  ;; sphinx-doc: add sphinx-doc comments easily
  ;; https://github.com/naiquevin/sphinx-doc.el
  (use-package sphinx-doc
    :diminish sphinx-doc-mode
    :config (add-hook 'python-mode-hook 'sphinx-doc-mode))

  ;; python-docstring: format and highlight syntax for python docstrings
  ;; https://github.com/glyph/python-docstring-mode
  (use-package python-docstring
    :diminish python-docstring-mode
    :config (add-hook 'python-mode-hook 'python-docstring-mode)))

(provide 'setup-python)

;; python anaconda-mode config
;; `C-c C-t' to show documentation of the thing at point
;; `M-.' to jump to the definition of a function
;; `M-,' to jump back from definition of a function
;; yapf will automatically format the buffer on save :)
;; to add sphinx-docs to a function, press `C-c M-d' on a function definition
