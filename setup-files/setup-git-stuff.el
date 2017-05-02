;; Time-stamp: <2017-05-02 11:11:55 csraghunandan>

;; https://magit.vc , https://github.com/magit/magit
;; magit: the git porcelain to manage git
(use-package magit
  :bind (("C-c m s" . magit-status)
          ("C-c m b" . magit-blame))
  :config (setq magit-completing-read-function 'ivy-completing-read)

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
      (switch-to-buffer chosen-buf)))
  (bind-key "C-c m p" #'wh/switch-magit-status-buffer))

;; git-timemachine: to rollback to different commits of files
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine :defer t
  :diminish git-timemachine-mode "𝐓𝐦"
  :bind (("C-c g t" . git-timemachine-toggle)))

;; diff-hl: highlight diffs in the fringe
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  ;; integate diff-hl with magit
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  (bind-key "C-c h d"
            (defhydra diff-hl-hunk-hydra (:color red)
              ("p" diff-hl-previous-hunk "prev hunk")
              ("n" diff-hl-next-hunk "next hunk")
              ("d" diff-hl-diff-goto-hunk "goto hunk")
              ("r" diff-hl-revert-hunk "revert hunk")
              ("m" diff-hl-mark-hunk "mark hunk")
              ("q" nil "quit" :color blue))))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:use-magit-popup t)

  (bind-key "C-c g m" 'git-messenger:popup-message)
  (bind-key "m" 'git-messenger:copy-message git-messenger-map))

;; git-link: emacs package for getting the github/gitlab/bitbucket URL
;; https://github.com/sshaw/git-link
(use-package git-link
  :bind
  ("C-c g l" . git-link)
  ("C-c g c" . git-link-commit)
  ("C-c g h" . git-link-homepage))

;; git-modes: major modes for git config, ignore and attributes files
;; https://github.com/magit/git-modes
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitattributes-mode)

(provide 'setup-git-stuff)

;; diff-hl
;; C-x v [ -> diff-hl-previous-hunk
;; C-x v ] -> diff-hl-next-hunk
;; C-x v = -> diff-hl-goto-hunk
;; C-x v n -> diff-hl-revert-hunk
