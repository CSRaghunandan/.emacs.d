;;; setup-git.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-29 16:19:44 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; https://magit.vc , https://github.com/magit/magit
;; magit: the git porcelain to manage git
;; `magit-status' is bound to `C-x g' by default
(use-package magit
  :bind (("C-c v c" . magit-clone)
         :map magit-status-mode-map
         ("Q" . mu-magit-kill-buffers)
         (:map dired-mode-map
               ("l" . magit-dired-log)))
  :bind* (("C-c p v" . magit-status))
  :config
  ;; Enable the binding for magit-file=popup
  (global-magit-file-mode 1)

  ;; modify a few magit parameters
  (setq magit-diff-refine-hunk t ; show word granularity within diff hunks
        magit-section-visibility-indicator nil
        magit-refs-show-commit-count 'all)

  ;; show counts in magit-refs
  (setq magit-refs-show-commit-count 'all)

  ;; don't ask for unsaved buffers for every magit operation. Very annoying
  (setq  magit-save-repository-buffers 'dontask)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)

  ;; Refresh VC state when Magit refreshes the buffer to keep ibuffer-vc in sync
  (add-hook 'magit-refresh-buffer-hook #'vc-refresh-state)

  (setq magit-completing-read-function 'ivy-completing-read)

  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers))))

;; forge: Access Git forges for Magit
;; https://github.com/magit/forge
(use-package forge
    :after magit
    :bind ((:map forge-issue-section-map
                 ("C-c C-v" . forge-browse-topic))
           (:map forge-pullreq-section-map
                 ("C-c C-v" . forge-browse-topic))))

;; magit-todos: Show source file TODOs in Magit status buffer
;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-group-by
        '(magit-todos-item-first-path-component magit-todos-item-keyword magit-todos-item-filename)))

;; Transient commands: replaces the old magit-popup
(use-package transient :defer t
  :config (transient-bind-q-to-quit))

;; git-timemachine: to rollback to different commits of files
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine-toggle)))

;; diff-hl: highlight diffs in the fringe
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode)
  :bind (("C-c h d" . diff-hl-hunk-hydra/body))
  :config
  (defhydra diff-hl-hunk-hydra (:color red)
    ("p" diff-hl-previous-hunk "prev hunk")
    ("n" diff-hl-next-hunk "next hunk")
    ("d" diff-hl-diff-goto-hunk "goto hunk")
    ("r" diff-hl-revert-hunk "revert hunk")
    ("m" diff-hl-mark-hunk "mark hunk")
    ("q" nil "Quit" :color blue)))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :after popup
  :bind(("C-c g m" . git-messenger:popup-message))
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:use-magit-popup t)
  (setq git-messenger:show-detail t))

;; git-link: emacs package for getting the github/gitlab/bitbucket URL
;; https://github.com/sshaw/git-link
(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :bind
  ("C-c g l" . git-link)
  ("C-c g c" . git-link-commit)
  ("C-c g h" . git-link-homepage))

;; git-modes: major modes for git config, ignore and attributes files
;; https://github.com/magit/git-modes
(use-package gitignore-mode
  :mode ((".gitignore_global" . gitignore-mode)))
(use-package gitconfig-mode
  :defer t)
(use-package gitattributes-mode
  :defer t)

;; rigid-tabs.el: Rigidify and adjust the visual alignment of TABs
;; https://gitlab.com/wavexx/rigid-tabs.el
(use-package rigid-tabs
  :hook ((diff-mode-hook . rigid-tabs-diff-align)
         (magit-refresh-buffer-hook . rigid-tabs-diff-align)))

(provide 'setup-git)

;; diff-hl
;; C-x v [ -> diff-hl-previous-hunk
;; C-x v ] -> diff-hl-next-hunk
;; C-x v = -> diff-hl-goto-hunk
;; C-x v n -> diff-hl-revert-hunk
;;
;; magit
;; |---------+----------------------------------|
;; | Binding | Description                      |
;; |---------+----------------------------------|
;; | j n     | Jump to Untracked section        |
;; | j u     | Jump to Unstaged section         |
;; | j s     | Jump to Staged section           |
;; | j p     | Jump to Unpushed section         |
;; | M-p     | Jump to previous sibling section |
;; | M-n     | Jump to next sibling section     |
;; | !!      | run a raw git command            |
;; |---------+----------------------------------|
;;
;; Tip: Adding prefix to above jump commands also expands those sections and
;; brings that section to the top of the buffer.
;;   So `C-u j s' is analogous to doing `j s C-l C-l 4`
;;
;; magit-edit-line-commit' and
;; `magit-diff-edit-hunk-commit', which allow editing the commit that
;; added the line at point.
;;
;; when in `magit-blame', press 'c' to cycle between styles
;;
;; in git-timemachine, press 't' to select the revision of the file based on
;; commit message
;;
;; `magit-log-trace-definition' -> Find commits affecting a function
