;; Time-stamp: <2017-01-02 20:12:58 csraghunandan>

;; magit, git-timemachine, diff-hl

;; https://magit.vc , https://github.com/magit/magit
;; magit - the git porcelain to manage git
(use-package magit :defer t
  :bind (("C-c m s" . magit-status)
          ("C-c m b"   . magit-blame))
  :config (setq magit-completing-read-function 'ivy-completing-read))

;; git-timemachine - to rollback to different commits of files
;; https://github.com/pidu/git-timemachine
(use-package git-timemachine :defer t
  :diminish git-timemachine-mode "𝐓𝐦"
  :bind (("C-c g t" . git-timemachine-toggle))
  :config
  (defun my-git-timemachine-show-selected-revision ()
    "Show last (current) revision of file."
    (interactive)
    (let* ((collection (mapcar (lambda (rev)
                                 ;; re-shape list for the ivy-read
                                 (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                               (git-timemachine--revisions))))
      (ivy-read "commits:"
                collection
                :action (lambda (rev)
                          ;; compatible with ivy 9+ and ivy 8
                          (unless (string-match-p "^[a-z0-9]*$" (car rev))
                            (setq rev (cdr rev)))
                          (git-timemachine-show-revision rev)))))

  (defun my-git-timemachine ()
    "Open git snapshot with the selected version.  Based on ivy-mode."
    (interactive)
    (unless (featurep 'git-timemachine)
      (require 'git-timemachine))
    (git-timemachine--start #'my-git-timemachine-show-selected-revision)))

;; diff-hl - highlight diffs in the fringe
;; https://github.com/dgutov/diff-hl
(use-package diff-hl :defer 1
  :config (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger :defer 1
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:use-magit-popup t)

  (bind-key "C-c g m" 'git-messenger:popup-message)
  (bind-key "m" 'git-messenger:copy-message git-messenger-map))

(provide 'setup-git-stuff)
