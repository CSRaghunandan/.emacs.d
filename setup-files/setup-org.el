;; Time-stamp: <2016-10-07 13:04:40 csraghunandan>

;; Org-mode configuration
;; http://orgmode.org/
(use-package org
  :preface
  (progn
    ;; If `org-load-version-dev' is non-nil, remove the older versions of org
    ;; from the `load-path'.
    (when (bound-and-true-p org-load-version-dev)
      (>=e "25.0" ; `directory-files-recursively' is not available in older emacsen
           (let ((org-stable-install-path (car (directory-files-recursively
                                                package-user-dir
                                                "org-plus-contrib-[0-9]+"
                                                :include-directories))))
             (setq load-path (delete org-stable-install-path load-path))
             ;; Also ensure that the associated path is removed from Info search list
             (setq Info-directory-list (delete org-stable-install-path
                                               Info-directory-list))

             ;; Also delete the path to the org directory that ships with emacs
             (dolist (path load-path)
               (when (string-match-p (concat "emacs/"
                                             (replace-regexp-in-string
                                              "\\.[0-9]+\\'" "" emacs-version)
                                             "/lisp/org\\'")
                                     path)
                 (setq load-path (delete path load-path)))))))

    ;; Modules that should always be loaded together with org.el.
    ;; `org-modules' default: '(org-w3m org-bbdb org-bibtex org-docview org-gnus
    ;;                          org-info org-irc org-mhe org-rmail)
    (setq org-modules '(org-info))

    ;; Set my default org-export backends. This variable needs to be set before
    ;; org.el is loaded.
    (setq org-export-backends '(ascii html latex))
    ;; Do not open links of mouse left clicks.
    ;; Default behavior caused inline images in org buffers to pop up in their
    ;; own buffers when left clicked on by mistake. I can still intentionally
    ;; open links and such images in new buffers by doing C-c C-o.
    (setq org-mouse-1-follows-link nil))

  :mode ("\\.org\\'" . org-mode)

  :config
  (progn
    ;; set org-agenda files folder
    (setq org-agenda-files (quote ("~/Org-mode files")))
    ;; (setq org-babel-python-command "python3")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (haskell . t)
       (js . t)
       (python . t)))

;;; Org Variables

    ;; Non-nil means insert state change notes and time stamps into a drawer.
    (setq org-log-into-drawer t)
    ;; insert a note after changing deadline for a TODO
    (setq org-log-redeadline 'note)
    ;; insert a note after rescheduling a TODO
    (setq org-log-reschedule 'note)
    ;; Insert only timestamp when closing an org TODO item
    (setq org-log-done 'timestamp)

    ;; refile settings
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-use-outline-path 'file)
    (setq org-refile-targets '(org-agenda-files :level . 1))

    (setq org-agenda-archives-mode nil) ; required in org 8.0+
    (setq org-agenda-skip-comment-trees nil)
    (setq org-agenda-skip-function nil)

    ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
    (setq org-pretty-entities t)

    ;; Render subscripts and superscripts in org buffers
    (setq org-pretty-entities-include-sub-superscripts t)
    ;; Allow _ and ^ characters to sub/super-script strings but only when
    ;; string is wrapped in braces
    (setq org-use-sub-superscripts '{}) ; in-buffer rendering

    (setq org-use-speed-commands t) ; ? speed-key opens Speed Keys help
    (setq org-speed-commands-user '(("m" . org-mark-subtree)))
    ;; heading leading stars for headlines
    (setq org-hide-leading-stars t)

    ;; Prevent auto insertion of blank lines before headings and list items
    (setq org-blank-before-new-entry '((heading)
                                       (plain-list-item)))

    ;; fold / overview  - collapse everything, show only level 1 headlines
    ;; content          - show only headlines
    ;; nofold / showall - expand all headlines except the ones with :archive:
    ;;                    tag and property drawers
    ;; showeverything   - same as above but without exceptions
    (setq org-startup-folded 'showall)

    ;; enable org-indent mode on startup
    (setq org-startup-indented t)
    ;; make sure org mode starts with flyspell mode enabled
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'auto-fill-mode)

    ;; strike through done headlines
    (setq org-fontify-done-headline t)

    ;; code to make jump to headline work. C-c C-j.
    (setq org-goto-interface 'outline-path-completion
          org-goto-max-level 10)

    ;; Block entries from changing state to DONE while they have children
    ;; that are not DONE - http://orgmode.org/manual/TODO-dependencies.html
    (setq org-enforce-todo-dependencies t)

    ;; http://emacs.stackexchange.com/a/17513/115
    (setq org-special-ctrl-a/e '(t ; For C-a. Possible values: nil, t, 'reverse
                                 . t)) ; For C-e. Possible values: nil, t, 'reverse
    (setq org-catch-invisible-edits 'smart) ; http://emacs.stackexchange.com/a/2091/115
    (setq org-indent-indentation-per-level 1) ; default = 2

    ;; Prevent renumbering/sorting footnotes when a footnote is added/removed.
    ;; Doing so would create a big diff in an org file containing lot of
    ;; footnotes even if only one footnote was added/removed.
    (setq org-footnote-auto-adjust nil)

    ;; Do NOT try to auto-evaluate entered text as formula when I begin a field's
    ;; content with "=" e.g. |=123=|. More often than not, I use the "=" to
    ;; simply format that field text as verbatim. As now the below variable is
    ;; set to nil, formula will not be automatically evaluated when hitting TAB.
    ;; But you can still using ‘C-c =’ to evaluate it manually when needed.
    (setq org-table-formula-evaluate-inline nil) ; default = t

    ;; http://sachachua.com/blog/2013/01/emacs-org-task-related-keyboard-shortcuts-agenda/
    (defun sacha/org-agenda-done (&optional arg)
      "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
      (interactive "P")
      (org-agenda-todo "DONE"))

    (defun sacha/org-agenda-mark-done-and-add-followup ()
      "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
      (interactive)
      (org-agenda-todo "DONE")
      (org-agenda-switch-to)
      (org-capture 0 "t")
      (org-metadown 1)
      (org-metaright 1))

    (defun sacha/org-agenda-new ()
      "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
      (interactive)
      (org-agenda-switch-to)
      (org-capture 0))

    ;; add key bindings for agenda mode
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (bind-keys
                 :map org-agenda-mode-map
                 ("x" . sacha/org-agenda-done)
                 ("X" . sacha/org-agenda-mark-done-and-add-followup)
                 ("N" . sacha/org-agenda-new))))

    ;;; Org Goto
    (defun modi/org-goto-override-bindings (&rest _)
      "Override the bindings set by `org-goto-map' function."
      (org-defkey org-goto-map "\C-p" #'outline-previous-visible-heading)
      (org-defkey org-goto-map "\C-n" #'outline-next-visible-heading)
      (org-defkey org-goto-map "\C-f" #'outline-forward-same-level)
      (org-defkey org-goto-map "\C-b" #'outline-backward-same-level)
      org-goto-map)
    (advice-add 'org-goto-map :after #'modi/org-goto-override-bindings)

    ;; Heading▮   --(C-c C-t)--> * TODO Heading▮
    ;; * Heading▮ --(C-c C-t)--> * TODO Heading▮
    (defun modi/org-first-convert-to-heading (orig-fun &rest args)
      (let ((is-heading))
        (save-excursion
          (forward-line 0)
          (when (looking-at "^\\*")
            (setq is-heading t)))
        (unless is-heading
          (org-toggle-heading))
        (apply orig-fun args)))
    (advice-add 'org-todo :around #'modi/org-first-convert-to-heading)

    ;; Bind the "org-table-*" command ONLY when the point is in an org table.
    (bind-keys
     :map org-mode-map
     :filter (org-at-table-p)
     ("C-c ?" . org-table-field-info)
     ("C-c SPC" . org-table-blank-field)
     ("C-c +" . org-table-sum)
     ("C-c =" . org-table-eval-formula)
     ("C-c `" . org-table-edit-field)
     ("C-#" . org-table-rotate-recalc-marks)
     ("C-c }" . org-table-toggle-coordinate-overlays)
     ("C-c {" . org-table-toggle-formula-debugger))

    (bind-keys
     ("C-c a" . org-agenda)
     ("C-c c" . org-capture)
     ("C-c i" . org-store-link))

    (use-package org-journal)

    (custom-set-faces
     '(org-document-title ((t (:height 1.3))))
     '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
     '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
     '(org-level-1 ((t (:inherit outline-1 :foreground "#DFAF8F" :height 1.2))))
     '(org-level-2 ((t (:inherit outline-2 :foreground "#BFEBBF" :height 1.15))))
     '(org-level-3 ((t (:inherit outline-3 :foreground "#7CB8BB" :height 1.1))))
     '(org-level-4 ((t (:inherit outline-4 :foreground "#D0BF8F" :height 1.05))))
     '(org-level-5 ((t (:inherit outline-5 :foreground "#93E0E3" :height 1)))))))

(provide 'setup-org)
