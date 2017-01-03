;; Time-stamp: <2017-01-03 10:32:57 csraghunandan>

;; Org-mode configuration - Make sure you install the latest org-mode with `M-x' RET `org-plus-contrib'
;; http://orgmode.org/
(use-package org
  :preface
  ;; If `org-load-version-dev' is non-nil, remove the older versions of org
  ;; from the `load-path'.
  (when (bound-and-true-p org-load-version-dev)
    (>= "25.0" ; `directory-files-recursively' is not available in older emacsen
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
  (setq org-mouse-1-follows-link nil)



  :mode ("\\.org\\'" . org-mode)

  :config
  ;; (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (js . t)
     (python . t)))

;;; Org Variables
  ;; default of 100 fill-column for org-mode
  (add-hook 'org-mode-hook (lambda()
                             (setq-local fill-column 100)))
  ;; this looks better in my opinion
  (setq org-ellipsis " ")
  ;; no underlines for org-ellipse
  (set-face-attribute 'org-ellipsis nil :underline nil :foreground "#E0CF9F")
  ;; hide emphasis markup characters
  (setq org-hide-emphasis-markers t)
  ;; Non-nil means insert state change notes and time stamps into a drawer.
  (setq org-log-into-drawer t)
  ;; insert a note after changing deadline for a TODO
  (setq org-log-redeadline 'note)
  ;; insert a note after rescheduling a TODO
  (setq org-log-reschedule 'note)
  ;; Insert only timestamp when closing an org TODO item
  (setq org-log-done 'timestamp)

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
  (use-package org-indent :ensure nil
    :diminish (org-indent-mode . "𝐈"))

  ;; strike through done headlines
  (setq org-fontify-done-headline t)

  ;; code to make jump to headline work. C-c C-j.
  (setq org-goto-interface 'outline-path-completion)
  ;; org-goto-max-level 10)

  ;; Block entries from changing state to DONE while they have children
  ;; that are not DONE - http://orgmode.org/manual/TODO-dependencies.html
  (setq org-enforce-todo-dependencies t)

  ;; http://emacs.stackexchange.com/a/17513/115
  (setq org-special-ctrl-a/e '(t ; For C-a. Possible values: nil, t, 'reverse
                               . t)) ; For C-e. Possible values: nil, t,
                                        ; 'reverse
  ;; special keys for killing headline
  (setq org-special-ctrl-k t)
  ;; don't split items when pressing `C-RET'. Always create new item
  (setq org-M-RET-may-split-line nil)
  ;; preserve indentation inside of source blocks
  (setq org-src-preserve-indentation t)

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

  ;; imenu should use a depth of 3 instead of 2
  (setq org-imenu-depth 3)

  ;; blank lines are removed when exiting code edit buffer
  (setq org-src-strip-leading-and-trailing-blank-lines t)



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

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "WAITING(w)" "|" "CANCELED(c)" "DONE(d)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("NEXT" . (:foreground "black" :weight bold :background "cyan"))
          ("WAITING" . (:foreground "black" :weight bold :background "pink"))
          ("SOMEDAY"  . (:foreground "black" :weight bold :background "#FFEF9F"))
          ("CANCELED" . (:foreground "red" :weight bold :strike-through t))
          ("DONE"     . (:foreground "black" :weight bold :background "#91ba31"))))

  (use-package langtool :defer 1
    :config
    ;; place the language-tool directory in $HOME
    (setq langtool-language-tool-jar (concat user-home-directory "LanguageTool-3.5/languagetool-commandline.jar"))
    (setq langtool-default-language "en-GB")

    ;; hydra for langtool check
    (defhydra hydra-langtool (:color pink
                                     :hint nil)
      "
_c_: check    _n_: next error
_C_: correct  _p_: prev error _d_: done checking
"
      ("n"  langtool-goto-next-error)
      ("p"  langtool-goto-previous-error)
      ("c"  langtool-check)
      ("C"  langtool-correct-buffer)
      ("d"  langtool-check-done :color blue)
      ("q" nil "quit" :color blue))
    (bind-key "C-c h l" 'hydra-langtool/body org-mode-map))



  (define-key org-mode-map "\"" #'endless/round-quotes)

  (defun endless/round-quotes (italicize)
    "Insert “” and leave point in the middle.
With prefix argument ITALICIZE, insert /“”/ instead
\(meant for org-mode).
Inside a code-block, just call `self-insert-command'."
    (interactive "P")
    (if (and (derived-mode-p 'org-mode)
             (org-in-block-p '("src" "latex" "html")))
        (call-interactively #'self-insert-command)
      (if (looking-at "”[/=_\\*]?")
          (goto-char (match-end 0))
        (when italicize
          (if (derived-mode-p 'markdown-mode)
              (insert "__")
            (insert "//"))
          (forward-char -1))
        (insert "“”")
        (forward-char -1))))

  (define-key org-mode-map "'" #'endless/apostrophe)

  (defun endless/apostrophe (opening)
    "Insert ’ in prose or `self-insert-command' in code.
With prefix argument OPENING, insert ‘’ instead and
leave point in the middle.
Inside a code-block, just call `self-insert-command'."
    (interactive "P")
    (if (and (derived-mode-p 'org-mode)
             (org-in-block-p '("src" "latex" "html")))
        (call-interactively #'self-insert-command)
      (if (looking-at "['’][=_/\\*]?")
          (goto-char (match-end 0))
        (if (null opening)
            (insert "’")
          (insert "‘’")
          (forward-char -1)))))



  ;; export to github flavored markdown
  (use-package ox-gfm :defer 5
    :config (require 'ox-gfm))

  ;; pomodoro implementation in org
  ;; https://github.com/lolownia/org-pomodoro
  (use-package org-pomodoro
    :bind ("C-c o p" . org-pomodoro)))

;; A journaling tool with org-mode: `org-journal'
;; https://github.com/bastibe/org-journal
;; Quick summary:
;; To create a new journal entry: C-c C-j
;; To open today’s journal without creating a new entry: C-u C-c C-j
;; In calendar view:
;; * j to view an entry in a new buffer
;; * C-j to view an entry but not switch to it
;; * i j to add a new entry
;; * f w to search in all entries of the current week
;; * f m to search in all entries of the current month
;; * f y to search in all entries of the current year
;; * f f to search in all entries of all time
;; * [ to go to previous entry
;; * ] to go to next ;entr
;; When viewing a journal entry:
;; * C-c C-f to view next entry
;; * C-c C-b to view previous entry
(use-package org-journal :defer 2
  :config
  (bind-key "C-c o j" 'org-journal-new-entry))

(provide 'setup-org)
