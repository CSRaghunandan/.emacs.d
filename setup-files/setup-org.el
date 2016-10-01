(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-agenda-archives-mode nil) ; required in org 8.0+
(setq org-log-done t)
(add-hook 'org-mode-hook 'flyspell-mode)
(setq org-agenda-files (quote ("~/Org-mode files")))
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (haskell . t)
   (js . t)
   (python . t)))

;; strike through done headlines
(setq org-fontify-done-headline t)

;; code to make jump to headline work. C-c C-j.
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
;; Render subscripts and superscripts in org buffers
(setq org-pretty-entities-include-sub-superscripts t)
;; Allow _ and ^ characters to sub/super-script strings but only when
;; string is wrapped in braces
(setq org-use-sub-superscripts '{}) ; in-buffer rendering
(setq org-use-speed-commands t) ; ? speed-key opens Speed Keys help
(setq org-speed-commands-user '(("m" . org-mark-subtree)))
(setq org-pretty-entities t)
(setq org-hide-leading-stars t)
;; expand all headlines on startup
(setq org-startup-folded 'showall)
(setq org-startup-indented t)
;; Block entries from changing state to DONE while they have children
;; that are not DONE - http://orgmode.org/manual/TODO-dependencies.html
(setq org-enforce-todo-dependencies t)
;; http://emacs.stackexchange.com/a/17513/115
(setq org-special-ctrl-a/e '(t ; For C-a. Possible values: nil, t, 'reverse
                             . t)) ; For C-e. Possible values: nil, t, 'reverse
(setq org-catch-invisible-edits 'smart) ; http://emacs.stackexchange.com/a/2091/115
;; Prevent renumbering/sorting footnotes when a footnote is added/removed.
;; Doing so would create a big diff in an org file containing lot of
;; footnotes even if only one footnote was added/removed.
(setq org-footnote-auto-adjust nil)

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

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (bind-keys
             :map org-agenda-mode-map
             ("x" . sacha/org-agenda-done)
             ("X" . sacha/org-agenda-mark-done-and-add-followup)
             ("N" . sacha/org-agenda-new))))

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

(use-package org-journal)

(provide 'setup-org)
