(use-package magit
  :bind* (("C-c m s" . magit-status)
          ("C-c m b"   . magit-blame))
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-timemachine
  :commands (git-timemachine-toggle
             git-timemachine-switch-branch)
  :bind* (("C-c t m" . git-timemachine-toggle)
          ("C-c t s" . git-timemachine-switch-branch)))

(use-package diff-hl :defer t
  :config (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(provide 'setup-git-stuff)
