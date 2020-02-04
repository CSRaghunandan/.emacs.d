;;; setup-org.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-04 17:23:59 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; Org-mode configuration - Make sure you install the latest org-mode with `M-x' RET `org-plus-contrib'
;; http://orgmode.org/
(use-package org
  :ensure nil
  :hook
  ((org-mode . org-num-mode)
   (org-mode . (lambda () ;; this will make sure auto-fill works for org-mode
                 (setq-local comment-auto-fill-only-comments nil)
                 (setq-local display-line-numbers-type 'absolute))))
  :preface
  ;; Modules that should always be loaded together with org.el.
  ;; `org-modules' default: (ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info
  ;;                         ol-irc ol-mhe ol-rmail ol-eww)
  (setq org-modules '(ol-info ol-irc org-habit ol-gnus))

  ;; Set my default org-export backends. This variable needs to be set before
  ;; org.el is loaded.
  (setq org-export-backends '(ascii html latex md gfm odt))

  :config

  ;; org agenda files
  (setq org-agenda-files '("~/org/inbox.org"
                           "~/org/gtd.org"
                           "~/org/tickler.org"
                           "~/org/projects.org"))

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-habit-show-habits t)

  ;; set default tags for org
  (setq org-tag-persistent-alist '(("@EMAIL" . ?e)
                                   ("@WRITE" . ?W)
                                   ("@PHONE" . ?p)
                                   ("@CONFIGURE" . ?C)
                                   ("@WORK" . ?w)
                                   ("@PERSONAL" . ?l)
                                   ("@LEARN" . ?n)
                                   ("@READ" . ?r)
                                   ("@WATCH" . ?W)
                                   ("@COMPUTER" . ?c)
                                   ("@BILLS" . ?b)
                                   ("@PURCHASE" . ?P)))

  ;; org capture templates
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/org/inbox.org" "Tasks")
                                 (file "~/.emacs.d/org-capture-templates/todo.txt"))
                                ("r" "Tickler" entry
                                 (file+headline "~/org/tickler.org" "Tickler")
                                 (file "~/.emacs.d/org-capture-templates/ticker.txt"))
                                ("b" "Add a book to read list" entry
                                 (file+headline "~/org/inbox.org" "Read list")
                                 (file "~/.emacs.d/org-capture-templates/book.txt"))
                                ("p" "Add a new project" entry
                                 (file+headline "~/org/projects.org" "Projects")
                                 (file "~/.emacs.d/org-capture-templates/projects.txt"))
                                ("n" "Note" entry
                                 (file "") ;empty string defaults to `org-default-notes-file'
                                 "\n* %?\n  Context:\n    %i\n  Entered on %U")))

  ;; settings for org-refile
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 3)
                             ("~/org/projects.org" :maxlevel . 3)))

  (with-eval-after-load "ivy"
    (add-to-list
    'ivy-completing-read-handlers-alist
    '(org-capture-refile . completing-read-default)))

  ;; override `avy-goto-char-timer' to C-' in org-mode-map
  (bind-key "C-'" 'avy-goto-char-timer org-mode-map)
  (bind-key "C-c C-/" #'org-refile org-mode-map)

  (bind-key "C-c M-a" #'ace-link-org org-mode-map)
  ;; add a tag to make ordered tasks more visible
  (setq org-track-ordered-property-with-tag t)

  ;; make sure all checkboxes under a todo is done before marking the parent
  ;; task as done.
  (setq org-enforce-todo-checkbox-dependencies t)

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)

  ;; clock into a drawer called CLOCKING
  (setq org-clock-into-drawer "CLOCKING")

  ;; Show drawers, e.g. :PROPERTIES:, when we expand a heading.
  ;; See http://emacs.stackexchange.com/a/22540/304
  (remove-hook 'org-cycle-hook #'org-cycle-hide-drawers)

  ;; ob-http: make http requests with org-mode babel
  ;; https://github.com/zweifisch/ob-http
  (use-package ob-http)

  ;; plantuml configuration
  (use-package ob-plantuml :ensure nil
    :commands
    (org-babel-execute:plantuml)
    :config
    (setq org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar")))

  ;;  Supercharge your Org daily/weekly agenda by grouping items
  ;; https://github.com/alphapapa/org-super-agenda
  (use-package org-super-agenda)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (css . t)
     (http . t)
     (C . t)
     (haskell . t)
     (js . t)
     (python . t)
     (plantuml . t)))

  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

;;; Org Variables

  ;; Non-nil means insert state change notes and time stamps into a drawer.
  (setq org-log-into-drawer t)
  ;; insert a note after changing deadline for a TODO
  (setq org-log-redeadline 'note)
  ;; insert a note after rescheduling a TODO
  (setq org-log-reschedule 'note)
  ;; Insert only timestamp when closing an org TODO item
  (setq org-log-done 'time)

  ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
  (setq org-pretty-entities t)

  ;; highlight latex stuff
  (setq org-highlight-latex-and-related '(latex))

  ;; Render subscripts and superscripts in org buffers
  (setq org-pretty-entities-include-sub-superscripts t)
  ;; Allow _ and ^ characters to sub/super-script strings but only when
  ;; string is wrapped in braces
  (setq org-use-sub-superscripts '{}) ; in-buffer rendering

  (setq org-use-speed-commands t) ; ? speed-key opens Speed Keys help
  (setq org-speed-commands-user '(("m" . org-mark-subtree)))
  ;; heading leading stars for headlines
  (setq org-hide-leading-stars t)

  ;; fold / overview  - collapse everything, show only level 1 headlines
  ;; content          - show only headlines
  ;; nofold / showall - expand all headlines except the ones with :archive:
  ;;                    tag and property drawers
  ;; showeverything   - same as above but without exceptions
  (setq org-startup-folded 'showall)

  ;; Prevent auto insertion of blank lines before headings and list items
  (setq org-blank-before-new-entry '((heading)
                                     (plain-list-item)))

  (bind-key "C-c C-j" 'counsel-org-agenda-headlines org-mode-map)

  ;; make tabs act like they would in the major mode for the source block
  (setq org-src-tab-acts-natively t)

  ;; strike through done headlines
  (setq org-fontify-done-headline t)

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

  (setq org-default-notes-file "~/org/.notes.org")

  (setq org-catch-invisible-edits 'smart) ; http://emacs.stackexchange.com/a/2091/115
  (setq org-indent-indentation-per-level 1) ; default = 2

  (with-eval-after-load 'org-footnote
    ;; Prevent renumbering/sorting footnotes when a footnote is added/removed.
    ;; Doing so would create a big diff in an Org file containing lot of
    ;; footnotes even if only one footnote was added/removed.
    (setq org-footnote-auto-adjust nil)) ;`'sort' - only sort
                                        ;`'renumber' - only renumber
                                        ;`t' - sort and renumber
                                        ;`nil' - do nothing (default)

  ;; imenu should use a depth of 3 instead of 2
  (setq org-imenu-depth 3)

  ;; blank lines are removed when exiting code edit buffer
  (setq org-src-strip-leading-and-trailing-blank-lines t)

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

  ;; Recalculate all org tables in the buffer when saving.
  ;; http://emacs.stackexchange.com/a/22221/115
  ;; Thu Jul 14 17:06:28 EDT 2016 - kmodi
  ;; Do not enable the buffer-wide recalculation by default because if an org
  ;; buffer has an org-table formula (like "#+TBLFM: $1=@#-1"), a *Calc*
  ;; buffer is created when `org-table-recalculate-buffer-tables' is run each
  ;; time.
  (defvar-local modi/org-table-enable-buffer-wide-recalculation nil
    "When non-nil, all the org tables in the buffer will be recalculated when
saving the file.
This variable is buffer local.")
  ;; Mark `modi/org-table-enable-buffer-wide-recalculation' as a safe local
  ;; variable as long as its value is t or nil. That way you are not prompted
  ;; to add that to `safe-local-variable-values' in custom.el.
  (put 'modi/org-table-enable-buffer-wide-recalculation 'safe-local-variable #'booleanp)

  (defun modi/org-table-recalculate-buffer-tables (&rest args)
    "Wrapper function for `org-table-recalculate-buffer-tables' that runs
that function only if `modi/org-table-enable-buffer-wide-recalculation' is
non-nil.
Also, this function has optional ARGS that is needed for any function that is
added to `org-export-before-processing-hook'. This would be useful if this
function is ever added to that hook."
    (when modi/org-table-enable-buffer-wide-recalculation
      (org-table-recalculate-buffer-tables)))

  (defun modi/org-table-recalculate-before-save ()
    "Recalculate all org tables in the buffer before saving."
    (add-hook 'before-save-hook #'modi/org-table-recalculate-buffer-tables nil :local))
  (add-hook 'org-mode-hook #'modi/org-table-recalculate-before-save)

  (defun org-table-mark-field ()
    "Mark the current table field."
    (interactive)
    ;; Do not try to jump to the beginning of field if the point is already there
    (when (not (looking-back "|\\s-?"))
      (org-table-beginning-of-field 1))
    (set-mark-command nil)
    (org-table-end-of-field 1))

  (defhydra hydra-org-table-mark-field
    (:body-pre (org-table-mark-field)
               :color red
               :hint nil)
    "
   ^^      ^🠙^     ^^
   ^^      _p_     ^^
🠘 _b_  selection  _f_ 🠚          | org table mark ▯field▮ |
   ^^      _n_     ^^
   ^^      ^🠛^     ^^
"
    ("x" exchange-point-and-mark "exchange point/mark")
    ("f" (lambda (arg)
           (interactive "p")
           (when (eq 1 arg)
             (setq arg 2))
           (org-table-end-of-field arg)))
    ("b" (lambda (arg)
           (interactive "p")
           (when (eq 1 arg)
             (setq arg 2))
           (org-table-beginning-of-field arg)))
    ("n" next-line)
    ("p" previous-line)
    ("q" nil "cancel" :color blue))

  (bind-keys
   :map org-mode-map
   :filter (org-at-table-p)
   ("S-SPC" . hydra-org-table-mark-field/body))

  (bind-keys
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c i" . org-store-link))

  (setq org-todo-keywords '((sequence
                             "TODO(t@/!)"
                             "NEXT(n@/!)"
                             "SOMEDAY(s/!)"
                             "WAITING(w@/!)"
                             "DELEGATED(e@/!)"
                             "|" "CANCELED(c)"
                             "DONE(d@)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("NEXT" . (:foreground "CadetBlue3" :weight bold))
          ("WAITING" . (:foreground "Pink" :weight bold))
          ("SOMEDAY"  . (:foreground "#FFEF9F" :weight bold))
          ("DELEGATED" . (:inherit outline-2))
          ("CANCELED" . (:foreground "red" :weight bold :strike-through t))
          ("DONE"     . (:foreground "SeaGreen4" :weight bold))))

  ;; Counsel and Org tags
  (defun modi/counsel-org-tag (&optional option)
    "Set Org tags, or just align tags in current heading or whole buffer.
If OPTION is \\='(4), only align the tags in the whole buffer.
If OPTION is \\='(16), only align the tags in the current heading."
    (interactive "P")
    (cond
     ((equal '(4) option)
      (org-set-tags-command :just-align))
     ((equal '(16) option)
      (org-set-tags-command nil :align-only-current))
     (t
      (counsel-org-tag))))

  (use-package langtool :defer 1
    :config
    ;; place the language-tool directory in $HOME
    (setq langtool-language-tool-jar
          (concat user-home-directory "/usr/local/Cellar/languagetool/4.1/libexec/languagetool-commandline.jar"))
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

  ;; http://emacs.stackexchange.com/a/10712/115
  (defun modi/org-delete-link ()
    "Replace an org link of the format [[LINK][DESCRIPTION]] with DESCRIPTION.
If the link is of the format [[LINK]], delete the whole org link.
In both the cases, save the LINK to the kill-ring.
Execute this command while the point is on or after the hyper-linked org link."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (let ((search-invisible t) start end)
        (save-excursion
          (when (re-search-backward "\\[\\[" nil :noerror)
            (when (re-search-forward "\\[\\[\\(.*?\\)\\(\\]\\[.*?\\)*\\]\\]"
                                     nil :noerror)
              (setq start (match-beginning 0))
              (setq end   (match-end 0))
              (kill-new (match-string-no-properties 1)) ; Save link to kill-ring
              (replace-regexp "\\[\\[.*?\\(\\]\\[\\(.*?\\)\\)*\\]\\]" "\\2"
                              nil start end)))))))
  (bind-key "C-c d l" 'modi/org-delete-link org-mode-map)

  ;; Org Cliplink: insert the link in the clipboard as an org link. Adds the
  ;; title of the page as the description
  ;; https://github.com/rexim/org-cliplink
  (use-package org-cliplink
    :bind (:map org-mode-map
                ;; "C-c C-l" is bound to `org-insert-link' by default
                ;; "C-c C-L" is bound to `org-cliplink'
                ("C-c C-S-l" . org-cliplink)))

  ;; org-download: easily add images to org buffers
  ;; https://github.com/abo-abo/org-download
  (use-package org-download)

  ;; ox-gfm: export to github flavored markdown
  ;; https://github.com/larstvei/ox-gfm
  (use-package ox-gfm)

  ;; ox-twbs: Export org-mode docs as HTML compatible with Twitter Bootstrap.
  ;; https://github.com/marsmining/ox-twbs
  (use-package ox-twbs)

  ;; org-toc: toc-org is an Emacs utility to have an up-to-date table of
  ;; contents in the org files without exporting (useful primarily for readme
  ;; files on GitHub)
  ;; https://github.com/snosov1/toc-org
  (use-package toc-org)

  ;; pomodoro implementation in org
  ;; https://github.com/lolownia/org-pomodoro
  (use-package org-pomodoro
    :config (bind-key "C-c o p" #'org-pomodoro org-mode-map))

  ;; Make C-u C-return insert heading *at point* (not respecting content),
  ;; even when the point is directly after a list item.
  ;; Reason: http://lists.gnu.org/r/emacs-orgmode/2018-02/msg00368.html
  (defun modi/org-insert-heading-respect-content (&optional invisible-ok)
    "Insert heading with `org-insert-heading-respect-content' set to t.
With \\[universal-argument] prefix, insert Org heading directly at
point."
    (interactive)
    (let ((respect-content (unless current-prefix-arg
                             '(4))))
      (org-insert-heading respect-content invisible-ok)))
  (advice-add 'org-insert-heading-respect-content :override
              #'modi/org-insert-heading-respect-content)

  (defun bjm/org-headline-to-top ()
    "Move the current org headline to the top of its section"
    (interactive)
    ;; check if we are at the top level
    (let ((lvl (org-current-level)))
      (cond
       ;; above all headlines so nothing to do
       ((not lvl)
        (message "No headline to move"))
       ((= lvl 1)
        ;; if at top level move current tree to go above first headline
        (org-cut-subtree)
        (beginning-of-buffer)
        ;; test if point is now at the first headline and if not then
        ;; move to the first headline
        (unless (looking-at-p "*")
          (org-next-visible-heading 1))
        (org-paste-subtree))
       ((> lvl 1)
        ;; if not at top level then get position of headline level above
        ;; current section and refile to that position. Inspired by
        ;; https://gist.github.com/alphapapa/2cd1f1fc6accff01fec06946844ef5a5
        (let* ((org-reverse-note-order t)
               (pos (save-excursion
                      (outline-up-heading 1)
                      (point)))
               (filename (buffer-file-name))
               (rfloc (list nil filename nil pos)))
          (org-refile nil nil rfloc))))))

  (defun has-space-at-boundary-p (string)
    "Check whether STRING has any whitespace on the boundary.
Return 'left, 'right, 'both or nil."
    (let ((result nil))
      (when (string-match-p "^[[:space:]]+" string)
        (setq result 'left))
      (when (string-match-p "[[:space:]]+$" string)
        (if (eq result 'left)
	        (setq result 'both)
	      (setq result 'right)))
      result))

  (defun is-there-space-around-point-p ()
    "Check whether there is whitespace around point.
Return 'left, 'right, 'both or nil."
    (let ((result nil))
      (when (< (save-excursion
                 (skip-chars-backward "[:space:]"))
               0)
        (setq result 'left))
      (when (> (save-excursion
                 (skip-chars-forward "[:space:]"))
               0)
        (if (eq result 'left)
	        (setq result 'both)
	      (setq result 'right)))
      result))

  (defun set-point-before-yanking (string)
    "Put point in the appropriate place before yanking STRING."
    (let ((space-in-yanked-string (has-space-at-boundary-p string))
	      (space-at-point (is-there-space-around-point-p)))
      (cond ((and (eq space-in-yanked-string 'left)
		          (eq space-at-point 'left))
	         (skip-chars-backward "[:space:]"))
	        ((and (eq space-in-yanked-string 'right)
		          (eq space-at-point 'right))
	         (skip-chars-forward "[:space:]")))))

  (defun set-point-before-yanking-if-in-text-mode (string)
    "Invoke `set-point-before-yanking' in text modes."
    (when (derived-mode-p 'text-mode)
      (set-point-before-yanking string)))

  (advice-add
   'insert-for-yank
   :before
   #'set-point-before-yanking-if-in-text-mode)

  (defun bjm/org-agenda-item-to-top ()
    "Move the current agenda item to the top of the subtree in its file"
    (interactive)
    ;; save buffers to preserve agenda
    (org-save-all-org-buffers)
    ;; switch to buffer for current agenda item
    (org-agenda-switch-to)
    ;; move item to top
    (bjm/org-headline-to-top)
    ;; go back to agenda view
    (switch-to-buffer (other-buffer (current-buffer) 1))
    ;; refresh agenda
    (org-agenda-redo))

  ;; bind to key 1
  (bind-key "1" 'bjm/org-agenda-item-to-top org-agenda-mode-map)

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'file)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/CANCELLED" 'file))

  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
              (add-hook 'org-agenda-mode-hook
                        (lambda ()
                          (add-hook 'window-configuration-change-hook
                                    'org-agenda-align-tags nil t))))

  (defun rag/copy-id-to-clipboard()
    "Copy the ID property value to killring,
if no ID is there then create a new unique ID.
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
    (interactive)
    (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
      (setq mytmpid (funcall 'org-id-get-create))
      (kill-new mytmpid)
      (message "Copied %s to killring (clipboard)" mytmpid)))
  (bind-key "H-i" 'rag/copy-id-to-clipboard org-mode-map)

  (bind-key "C-c h c" 'hydra-org-clock/body org-mode-map)
  (defhydra hydra-org-clock (:color blue
                                    :hint nil)
    "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)   _i_n         _e_dit    _g_oto entry | (_z_)     _r_elative      ti_m_e
^ ^     _c_ontinue   _q_uit    _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
^ ^     _o_ut        ^ ^       _r_eport     |  ^ ^      _p_ause toggle
^ ^     ^ ^          ^ ^       ^ ^          |  ^ ^      _s_top
"
    ("i" org-clock-in)
    ("c" org-clock-in-last)
    ("o" org-clock-out)
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("?" (org-info "Clocking commands"))
    ("r" org-timer-start)
    ("n" org-timer-set-timer)
    ("p" org-timer-pause-or-continue)
    ("s" org-timer-stop)
    ("m" org-timer)
    ("t" org-timer-item)
    ("z" (org-info "Timers")))

  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-org.el#L330
  (defun modi/org-in-any-block-p ()
    "Return non-nil if the point is in any Org block.
The Org block can be *any*: src, example, verse, etc., even any
Org Special block.
This function is heavily adapted from `org-between-regexps-p'."
    (save-match-data
      (let ((pos (point))
            (case-fold-search t)
            (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
            (limit-up (save-excursion (outline-previous-heading)))
            (limit-down (save-excursion (outline-next-heading)))
            beg end)
        (save-excursion
          ;; Point is on a block when on BLOCK-BEGIN-RE or if
          ;; BLOCK-BEGIN-RE can be found before it...
          (and (or (org-in-regexp block-begin-re)
                   (re-search-backward block-begin-re limit-up :noerror))
               (setq beg (match-beginning 0))
               ;; ... and BLOCK-END-RE after it...
               (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                           (match-string-no-properties 1)
                                           "\\( .*\\)*$")))
                 (goto-char (match-end 0))
                 (re-search-forward block-end-re limit-down :noerror))
               (> (setq end (match-end 0)) pos)
               ;; ... without another BLOCK-BEGIN-RE in-between.
               (goto-char (match-beginning 0))
               (not (re-search-backward block-begin-re (1+ beg) :noerror))
               ;; Return value.
               (cons beg end))))))

  (defun modi/org-split-block ()
    "Sensibly split the current Org block at point.
(1) Point in-between a line
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message▮ \"one\")                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
                                       #+begin_src emacs-lisp
                                       (message \"two\")
                                       #+end_src
(2) Point at EOL
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")▮                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
                                       #+begin_src emacs-lisp
                                       (message \"two\")
                                       #+end_src
(3) Point at BOL
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")                    (message \"one\")
    ▮(message \"two\")          -->      #+end_src
    #+end_src                          ▮
                                       #+begin_src emacs-lisp
                                       (message \"two\")
                                       #+end_src
"
    (interactive)
    (if (modi/org-in-any-block-p)
        (save-match-data
          (save-restriction
            (widen)
            (let ((case-fold-search t)
                  (at-bol (bolp))
                  block-start
                  block-end)
              (save-excursion
                (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
                (setq block-start (match-string-no-properties 0))
                (setq block-end (replace-regexp-in-string
                                 "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                                 (match-string-no-properties 1))))
              ;; Go to the end of current line, if not at the BOL
              (unless at-bol
                (end-of-line 1))
              (insert (concat (if at-bol "" "\n")
                              block-end
                              "\n\n"
                              block-start
                              (if at-bol "\n" "")))
              ;; Go to the line before the inserted "#+begin_ .." line
              (beginning-of-line (if at-bol -1 0)))))
      (message "Point is not in an Org block")))

  (defun modi/org-meta-return (&optional arg)
    "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item',
`org-table-wrap-region', or `modi/org-split-block' depending on
context.  When called with an argument, unconditionally call
`org-insert-heading'."
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (or (run-hook-with-args-until-success 'org-metareturn-hook)
        (call-interactively (cond (arg #'org-insert-heading)
                                  ((org-at-table-p) #'org-table-wrap-region)
                                  ((org-in-item-p) #'org-insert-item)
                                  ((modi/org-in-any-block-p) #'modi/org-split-block)
                                  (t #'org-insert-heading)))))
  (advice-add 'org-meta-return :override #'modi/org-meta-return)

  ;; Make C-u C-return insert heading *at point* (not respecting content),
  ;; even when the point is directly after a list item.
  ;; Reason: http://lists.gnu.org/r/emacs-orgmode/2018-02/msg00368.html
  (defun modi/org-insert-heading-respect-content (&optional invisible-ok)
    "Insert heading with `org-insert-heading-respect-content' set to t.
With \\[universal-argument] prefix, insert Org heading directly at
point."
    (interactive)
    (let ((respect-content (unless current-prefix-arg
                             '(4))))
      (org-insert-heading respect-content invisible-ok)))
  (advice-add 'org-insert-heading-respect-content :override
              #'modi/org-insert-heading-respect-content))

;; archive subtrees/headings while also preserving their context
(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                           (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

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
  :bind (("C-c o j" . org-journal-new-entry))
  :hook ((org-journal-mode . (lambda ()
                               (visual-line-mode -1)))))

;; org-sticky-headers
;; https://github.com/alphaapapa/org-sticky-header
(use-package org-sticky-header
  :hook ((org-mode . org-sticky-header-mode))
  :config
  ;; show full path from the org-mode header
  (setq org-sticky-header-full-path 'full)
  (setq org-sticky-header-always-show-header
        (if org-sticky-header-full-path t nil)))

(provide 'setup-org)
