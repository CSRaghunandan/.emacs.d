;; Time-stamp: <2016-12-02 13:47:31 csraghunandan>

;; Python configuration
(use-package python
  :bind (:map python-mode-map
              (("C-c C-t" . anaconda-mode-show-doc)
               ("M-." . anaconda-mode-find-definitions)
               ("M-," . anaconda-mode-go-back-definitions)))
  :config
  (setq python-shell-interpreter "python3")
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)

  ;; anaconda-mode :- bring IDE like features for python-mode
  ;; https://github.com/proofit404/anaconda-mode
  (use-package anaconda-mode
    :diminish anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

  ;; company backend for anaconda-mode
  ;; https://github.com/proofit404/company-anaconda
  (use-package company-anaconda
    :config
    (defun my-anaconda-mode-hook ()
      "Hook for `web-mode'."
      (set (make-local-variable 'company-backends)
           '((company-anaconda company-files))))
    (add-hook 'python-mode-hook 'my-anaconda-mode-hook))

  ;; for testing python code
  ;; https://github.com/ionrock/pytest-el
  (use-package pytest :defer t)

  ;; yapfify: format python files automatically
  ;; https://github.com/JorisE/yapfify
  (use-package yapfify
    :diminish yapf-mode
    :config
    (add-hook 'python-mode-hook 'yapf-mode))

  ;; Add sphinx-doc comments easily
  ;; https://github.com/naiquevin/sphinx-doc.el
  (use-package sphinx-doc
    :diminish sphinx-doc-mode
    :config (add-hook 'python-mode-hook 'sphinx-doc-mode))

  ;; format and highlight syntax for python docstrings
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
