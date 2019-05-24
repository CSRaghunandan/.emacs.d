;;; setup-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-05-24 15:38:45 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

(use-package python
  :ensure nil
  :hook ((python-mode . (lambda ()
                          (lsp)
                          (lsp-ui-mode)
                          (eldoc-mode -1)
                          (lsp-ui-sideline-mode)
                          (lsp-ui-doc-mode)
                          (flycheck-mode)
                          (smart-dash-mode)
                          (company-mode)
                          (setq-local lsp-highlight-symbol-at-point nil)))
         (python-mode . (lambda ()
                          (setq-local tab-width 4)
                          (setq-local lsp-pyls-plugins-pylint-enabled nil)
                          (setq-local lsp-pyls-plugins-rope-completion-enabled nil)))
         (inferior-python-mode . company-mode))
  :config
  ;; don't try to guess python indent offset
  (setq python-indent-guess-indent-offset nil)
  (setq python-shell-completion-native-enable nil)

  (defun my-python-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'python-mode-hook #'my-python-mode-hook)

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
      (warn "python-mode: Cannot find autoflake executable, automatic removal of unused imports disabled")))

  (when (executable-find "yapf")
    (add-hook 'python-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (time-stamp)
                            (lsp-format-buffer)) nil t)))))

;; pytest: for testing python code
;; https://github.com/ionrock/pytest-el
(use-package pytest :defer t)

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

;; to get all the functionalities of thepython language server, install using
;; pip the below packages:
;;   python-language-server, Jedi, Pyflakes, McCabe, pycodestyle,
;;   pydocstyle, yapf, pyls-mypy, pyls-isort
