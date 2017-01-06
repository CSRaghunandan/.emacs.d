;; Time-stamp: <2017-01-05 23:27:54 csraghunandan>

;; magit, git-timemachine, diff-hl

;; https://magit.vc , https://github.com/magit/magit
;; magit - the git porcelain to manage git
(use-package magit
  :bind (("C-c m s" . magit-status)
          ("C-c m b" . magit-blame))
  :config (setq magit-completing-read-function 'ivy-completing-read))

;; git-timemachine - to rollback to different commits of files
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine :defer t
  :diminish git-timemachine-mode "𝐓𝐦"
  :bind (("C-c g t" . git-timemachine-toggle)))

;; diff-hl - highlight diffs in the fringe
;; https://github.com/dgutov/diff-hl
(use-package diff-hl :defer 1
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  ;; integate diff-hl with magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger :defer 1
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:use-magit-popup t)

  (bind-key "C-c g m" 'git-messenger:popup-message)
  (bind-key "m" 'git-messenger:copy-message git-messenger-map))

(provide 'setup-git-stuff)

;; diff-hl
;; C-x v [ -> diff-hl-previous-hunk
;; C-x v ] -> diff-hl-next-hunk
;; C-x v = -> diff-hl-goto-hunk
;; C-x v n -> diff-hl-revert-hunk
