;; Time-stamp: <2018-06-11 00:02:04 csraghunandan>

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
          treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face))

    ;; don't show files in .gitignore
    (setq treemacs-python-executable (executable-find "python3"))
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (defun treemacs--setup-mode-line ()
      "Create either a simple modeline, or integrate into spaceline."
      (setq mode-line-format
            (cond ((memq 'moody-mode-line-buffer-identification
                         (default-value 'mode-line-format))
                   '(:eval (moody-tab " Treemacs " 10 'down)))
                  (t
                   '(" Treemacs "))))))

  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-c f" . treemacs-select-window)))

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (:map global-map
              ("C-c o p" . treemacs-projectile)))

(provide 'setup-treemacs)
