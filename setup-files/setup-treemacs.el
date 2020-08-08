;;; setup-treemacs.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-08 11:47:58 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; treemacs: a tree layout file explorer for Emacs
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-recenter-after-file-follow t
          treemacs-width 40
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-eldoc-display nil
          treemacs-collapse-dirs (if (executable-find "python") 3 0)
          treemacs-silent-refresh t
          treemacs-eldoc-display t
          treemacs-silent-filewatch t
          treemacs-change-root-without-asking t
          treemacs-sorting 'alphabetic-asc
          treemacs-show-hidden-files t
          treemacs-never-persist nil
          treemacs-is-never-other-window t
          treemacs-user-mode-line-format 'none)

    ;; set the correct python3 executable path. This is needed for
    ;; treemacs-git-mode extended
    (setq treemacs-python-executable (executable-find "python"))

    ;; highlight current line in fringe for treemacs window
    (treemacs-fringe-indicator-mode)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))

  :bind
  (:map global-map
        ("C-c f" . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)))

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (:map global-map
              ("C-c o t" . treemacs-projectile)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;; Integration between lsp-mode and treemacs and implementation of treeview
;; controls using treemacs as a tree renderer.
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :after treemacs
  :config
  (lsp-treemacs-sync-mode 1))

;; treemacs theme using all-the-icons
(use-package treemacs-all-the-icons
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

(provide 'setup-treemacs)
