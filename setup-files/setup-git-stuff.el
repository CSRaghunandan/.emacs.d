;;; setup-git-stuff.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-01-07 13:48:59 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; https://magit.vc , https://github.com/magit/magit
;; magit: the git porcelain to manage git
;; `magit-status' is bound to `C-x g' by default
(use-package magit
  :bind (("C-c m p" . wh/switch-magit-status-buffer)
         ("C-c v c" . magit-clone)
         ("C-c p v" . magit-status)
         :map magit-status-mode-map
         ("Q" . mu-magit-kill-buffers)
         (:map magit-mode-map
               ([remap previous-line] . magit-previous-line)
               ([remap next-line] . magit-next-line))
         (:map dired-mode-map
               ("l" . magit-dired-log)))
  :config
  ;; Enable the binding for magit-file=popup
  (global-magit-file-mode 1)

  ;; show counts in magit-refs
  (setq magit-refs-show-commit-count 'all)

  ;; don't ask for unsaved buffers for every magit operation. Very annoying
  (setq  magit-save-repository-buffers 'dontask)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)

  ;; Refresh VC state when Magit refreshes the buffer to keep ibuffer-vc in sync
  (add-hook 'magit-refresh-buffer-hook #'vc-refresh-state)

  ;; Magit Submodule support
  ;; https://www.reddit.com/r/emacs/comments/6aiwk5/how_to_manage_multiple_gitrepositories_at_once/dhf47dg/
  (dolist (fn '(;; Below will end up being the last of these newly added fns,
                ;; and the last element in `magit-status-sections-hook' too.
                magit-insert-modules-unpulled-from-upstream
                magit-insert-modules-unpushed-to-pushremote
                magit-insert-modules-unpushed-to-upstream
                ;; Below will end up being the first of these newly added fns.
                magit-insert-modules-unpulled-from-pushremote))
    (magit-add-section-hook 'magit-status-sections-hook `,fn nil :append))

  (setq magit-completing-read-function 'ivy-completing-read)

  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  ;; show word granularity within diff hunks
  (setq-default magit-diff-refine-hunk 'all)

  (defun wh/switch-magit-status-buffer ()
    "Allow switching between open magit status buffers."
    (interactive)
    (let* ((buffers (--filter (eq #'magit-status-mode (with-current-buffer it major-mode))
                              (buffer-list)))
           (bufs-with-names (--map (cons
                                    (with-current-buffer it
                                      (projectile-project-name))
                                    it)
                                   buffers))
           (chosen-buf
            (cdr (assoc (completing-read "Git project: " bufs-with-names)
                        bufs-with-names))))
      (switch-to-buffer chosen-buf))))

;; Add more actions to magit file operations
(use-package magit-files
  :ensure magit
  :config
  ;; Add more operations to the file popup
  (magit-define-popup-action 'magit-file-popup
    ?R "Rename file" 'magit-file-rename)
  (magit-define-popup-action 'magit-file-popup
    ?K "Delete file" 'magit-file-delete)
  (magit-define-popup-action 'magit-file-popup
    ?U "Untrack file" 'magit-file-untrack)
  (magit-define-popup-action 'magit-file-popup
    ?C "Checkout file" 'magit-file-checkout))

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

;; git-timemachine: to rollback to different commits of files
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine-toggle)))

;; diff-hl: highlight diffs in the fringe
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
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
    ("q" nil "quit" :color blue)))

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

(use-package magit-log :ensure nil
  :init
  (progn
    ;; Set `magit-log-margin' value in :init as many other variables will be
    ;; dynamically set based on its value when `magit-log' is loaded.
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    ;; Show the commit ages with 1-char time units
    ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
    ;; Also reduce the author column width to 11 as the author name is being
    ;; abbreviated below.
    (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11)))
  :config
  (progn
    ;; Abbreviate author name. I added this so that I can view Magit log without
    ;; too much commit message truncation even on narrow screens (like on phone).
    (defun modi/magit-log--abbreviate-author (&rest args)
      "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).
It is assumed that the author has only one or two names."
      ;; ARGS               -> '((REV AUTHOR DATE))
      ;; (car ARGS)         -> '(REV AUTHOR DATE)
      ;; (nth 1 (car ARGS)) -> AUTHOR
      (let* ((author (nth 1 (car args)))
             (author-abbr (if (string-match-p "," author)
                              ;; Last, First -> F Last
                              (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                            ;; First Last -> F Last
                            (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
        (setf (nth 1 (car args)) author-abbr))
      (car args))                       ;'(REV AUTHOR-ABBR DATE)
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)))

;; git-modes: major modes for git config, ignore and attributes files
;; https://github.com/magit/git-modes
(use-package gitignore-mode
  :mode ((".gitignore_global" . gitignore-mode)))
(use-package gitconfig-mode
  :defer t)
(use-package gitattributes-mode
  :defer t)

;; gist: Yet another Emacs paste mode, this one for Gist.
;; https://github.com/defunkt/gist.el
(use-package gist
  :defer t)

;; gitflow extensions for magit
;; https://github.com/jtatarik/magit-gitflow/
(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow)
  :config
  ;; Free C-f and use a more suitable key binding
  (unbind-key "C-f" magit-gitflow-mode-map)
  (bind-key "C-c v f" #'magit-gitflow-popup magit-gitflow-mode-map))

;; rigid-tabs.el: Rigidify and adjust the visual alignment of TABs
;; https://gitlab.com/wavexx/rigid-tabs.el
(use-package rigid-tabs
  :hook ((diff-mode-hook . rigid-tabs-diff-align)
         (magit-refresh-buffer-hook . rigid-tabs-diff-align)))

(provide 'setup-git-stuff)

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
