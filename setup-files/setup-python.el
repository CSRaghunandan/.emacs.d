;; Time-stamp: <2017-05-12 00:57:25 csraghunandan>

;; Python configuration
(use-package python
  :bind (:map python-mode-map
              (("C-c C-t" . anaconda-mode-show-doc)
               ("M-." . anaconda-mode-find-definitions)
               ("M-," . anaconda-mode-go-back-definitions)))
  :config
  ;; don't try to guess python indent offset
  (setq python-indent-guess-indent-offset nil)

  (add-hook 'python-mode-hook (lambda ()
                                (company-mode)
                                (smart-dash-mode)
                                (flycheck-mode)
                                (page-break-lines-mode)))
  ;; enable company-mode completions in inferior python process
  (add-hook 'inferior-python-mode-hook 'company-mode)

  ;; anaconda-mode: bring IDE like features for python-mode
  ;; https://github.com/proofit404/anaconda-mode
  (use-package anaconda-mode
    :diminish (anaconda-mode . "𝐀𝐧")
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

  ;; pyenv: Python virtual environment interface for Emacs
  ;; https://github.com/jorgenschaefer/pyvenv
  (use-package pyvenv)

  ;; pyenv-mode: Integrate pyenv with python-mode.
  ;; https://github.com/proofit404/pyenv-mode
  (use-package pyenv-mode
    :if (executable-find "pyenv")
    :config

    (add-hook 'python-mode-hook 'pyenv-mode)

    ;; integrate pyenv with projectile
    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))

    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

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

  ;; only install yapfify if yapf is installed
  (use-package py-yapf
    :if (executable-find "yapf"))

  ;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
  (defun python-remove-unused-imports()
    "Use Autoflake to remove unused function"
    "autoflake --remove-all-unused-imports -i unused_imports.py"
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (message "Error: Cannot find autoflake executable.")))

  ;; py-isort: sort import statements in python buffers
  ;; https://github.com/paetzke/py-isort.el
  (use-package py-isort
    :if (executable-find "isort")
    :config
    (add-hook 'python-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (time-stamp)
                            (py-yapf-buffer)
                            (py-isort-buffer))))))

  ;; sphinx-doc: add sphinx-doc comments easily
  ;; https://github.com/naiquevin/sphinx-doc.el
  (use-package sphinx-doc
    :diminish sphinx-doc-mode
    :config (add-hook 'python-mode-hook 'sphinx-doc-mode))

  ;; pip-requirements: Major mode for editing pip requirements files
  ;; https://github.com/Wilfred/pip-requirements.el
  (use-package pip-requirements
    :config
    (add-hook 'pip-requirements-mode 'company-mode))

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
