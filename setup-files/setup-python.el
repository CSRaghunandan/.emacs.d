;;; setup-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-01-14 19:39:52 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              (("C-c C-t" . anaconda-mode-show-doc)
               ("M-." . anaconda-mode-find-definitions)
               ("M-," . anaconda-mode-go-back-definitions)))
  :hook ((python-mode .  (lambda ()
                           (company-mode)
                           (smart-dash-mode)
                           (flycheck-mode)
                           (setq-local tab-width 4)))
         (inferior-python-mode . company-mode))

  :config
  ;; don't try to guess python indent offset
  (setq python-indent-guess-indent-offset nil)

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
      (warn "python-mode: Cannot find autoflake executable, automatic removal of unused imports disabled"))))

;; pytest: for testing python code
;; https://github.com/ionrock/pytest-el
(use-package pytest :defer t)

;; format python buffers using yapf
;; https://github.com/JorisE/yapfify/tree/master
(use-package yapfify
  :hook ((python-mode . yapf-mode)))

;; anaconda-mode: bring IDE like features for python-mode
;; https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;; run multiple python linters parallely
;; https://github.com/msherry/flycheck-pycheckers
(use-package flycheck-pycheckers
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

;; company-anaconda: company backend for anaconda
;; https://github.com/proofit404/company-anaconda
(use-package company-anaconda
  :after anaconda-mode
  :config
  (defun my-anaconda-mode-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '((company-anaconda company-files company-yasnippet))))
  (add-hook 'python-mode-hook 'my-anaconda-mode-hook))

;; pyenv-mode: Integrate pyenv with python-mode.
;; https://github.com/proofit404/pyenv-mode
(use-package pyenv-mode
  :hook ((python-mode . pyenv-mode))
  :config
  ;; integrate pyenv with projectile
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

;; py-isort: sort import statements in python buffers
;; https://github.com/paetzke/py-isort.el
(use-package py-isort
  :if (executable-find "isort"))

;; python-docstring: format and highlight syntax for python docstrings
;; https://github.com/glyph/python-docstring-mode
(use-package python-docstring
  :hook ((python-mode . python-docstring-mode)))

;; pip-requirements: Major mode for editing pip requirements files
;; https://github.com/Wilfred/pip-requirements.el
(use-package pip-requirements
  :hook ((pip-requirements-mode . company-mode)))

;; sphinx-doc: add sphinx-doc comments easily
;; https://github.com/naiquevin/sphinx-doc.el
;; to add sphinx-docs to a function, press `C-c M-d' on a function definition
(use-package sphinx-doc
  :hook ((python-mode . sphinx-doc-mode)))

(provide 'setup-python)

;; python anaconda-mode config
;; `C-c C-t' to show documentation of the thing at point
;; `M-.' to jump to the definition of a function
;; `M-,' to jump back from definition of a function
;; yapf will automatically format the buffer on save :)
;; to add sphinx-docs to a function, press `C-c M-d' on a function definition
