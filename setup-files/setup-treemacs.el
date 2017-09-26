;; Time-stamp: <2017-09-26 18:43:25 csraghunandan>

;; treemacs: a tree layout file explorer for Emacs
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs :defer t
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-width 35
          treemacs-indentation 2
          treemacs-git-integration t
          treemacs-collapse-dirs 3
          treemacs-silent-refresh nil
          treemacs-change-root-without-asking nil
          treemacs-sorting 'alphabetic-desc
          treemacs-show-hidden-files t
          treemacs-never-persist nil
          treemacs-is-never-other-window t
          treemacs-goto-tag-strategy 'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))

  :bind
  (:map global-map
        ([f8] . treemacs-toggle)
        ("C-c s t" . treemacs-select-window)))

(use-package treemacs-projectile :defer t
  :config (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("C-c o p" . treemacs-projectile-toggle)))

(provide 'setup-treemacs)
