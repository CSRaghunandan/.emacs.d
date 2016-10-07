;; Time-stamp: <2016-10-07 13:06:20 csraghunandan>

;; magit, git-timemachine, diff-hl
;; https://magit.vc , https://github.com/magit/magit
;; magit - the git porcelain to manage git
(use-package magit :defer t
  :bind* (("C-c m s" . magit-status)
          ("C-c m b"   . magit-blame))
  :config (setq magit-completing-read-function 'ivy-completing-read))

;; git-timemachine
;; https://github.com/pidu/git-timemachine
;; to rollback to different commits of files
(use-package git-timemachine :defer t
  :commands (git-timemachine-toggle
             git-timemachine-switch-branch)
  :bind* (("C-c t m" . git-timemachine-toggle)
          ("C-c t s" . git-timemachine-switch-branch)))

;; diff-hl
;; https://github.com/dgutov/diff-hl
;; highlight diffs in the fringe
(use-package diff-hl :defer t
  :config (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(provide 'setup-git-stuff)
