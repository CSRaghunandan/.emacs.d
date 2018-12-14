;;; setup-treemacs.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-12-14 13:17:12 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; treemacs: a tree layout file explorer for Emacs
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-width 35
          treemacs-indentation 1
          treemacs-follow-after-init t
          treemacs-recenter-after-file-follow nil
          treemacs-collapse-dirs (if (executable-find "python") 3 0)
          treemacs-silent-refresh t
          treemacs-silent-filewatch t
          treemacs-change-root-without-asking t
          treemacs-sorting 'alphabetic-desc
          treemacs-show-hidden-files t
          treemacs-never-persist nil
          treemacs-is-never-other-window t
          treemacs-indentation-string (propertize " ǀ " 'face 'font-lock-comment-face))

    ;; set the correct python3 executable path. This is needed for
    ;; treemacs-git-mode extended
    (setq treemacs-python-executable (executable-find "python"))

    ;; don't show files in .gitignore
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))

  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-c f" . treemacs-select-window)))

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (:map global-map
              ("C-c o p" . treemacs-projectile)))

(provide 'setup-treemacs)
