;;; setup-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-12-30 01:14:55 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;;; configuration for all the editing stuff in emacs
;; Kill ring
(setq kill-ring-max 200
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t)

(defun rag/clear-kill-ring ()
  "clears the kill ring"
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))

(bind-key "C-c r c" 'rag/clear-kill-ring)

;; cutting and pasting uses primary clipboard
(setq select-enable-primary t)
(setq select-enable-clipboard t)

;; always insert spaces, do not insert tabs
(setq-default indent-tabs-mode nil)
;; set default tab width to 4
(setq-default tab-width 4)

(setq-default fill-column 80) ;; default is 70

;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces.
(setq-default sentence-end-double-space nil)

;; http://emacs.stackexchange.com/q/7519/115
(defun rag/pull-up-line ()
  "Join the following line onto the current one (analogous to `C-e', `C-d') or
`C-u M-^' or `C-u M-x join-line'.
If the current line is a comment and the pulled-up line is also a comment,
remove the comment characters from that line."
  (interactive)
  (join-line -1)
  ;; If the current line is a comment
  (when (nth 4 (syntax-ppss))
    ;; Remove the comment prefix chars from the pulled-up line if present
    (save-excursion
      ;; Delete all comment-start and space characters
      (while (looking-at (concat "\\s<" ; comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ; first char of `comment-start'
                                 "\\|" "\\s-")) ; extra spaces
        (delete-char 1))
      (insert-char ? )))) ; insert space

(defun rag/push-up-line()
  "Join the current line onto the previous one.
If the current line is comment and the previous line is also a comment, remove
the comment characters from the joined line."
  (interactive)
  (join-line)
  ;; If the current line is a comment
  (when (nth 4 (syntax-ppss))
    ;; Remove the comment prefix chars from the pulled-up line if present
    (save-excursion
      (forward-char)
      ;; Delete all comment-start or space characters
      (while (looking-at (concat "\\s<" ; comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ; first char of `comment-start'
                                 "\\|" "\\s-")) ; extra spaces
        (delete-forward-char 1)))))

(bind-keys
 ("M-j" . rag/pull-up-line)
 ("H-j" . rag/push-up-line))

(defun rag/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun rag/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(bind-keys
 ("C-o" . rag/smart-open-line)
 ("C-S-o" . rag/smart-open-line-above))



;; operate on current line if no region is defined
;; https://github.com/purcell/whole-line-or-region/blob/master/whole-line-or-region.el
(use-package whole-line-or-region
 :config (whole-line-or-region-global-mode))



(defun rag/select-inside-line ()
  "Select the current line."
  (interactive)
  (mwim-beginning-of-code-or-line)
  (set-mark (line-end-position))
  (exchange-point-and-mark))
(bind-key "C-c l" 'rag/select-inside-line)

;; align commands
(defun rag/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 1 t))

(defun rag/align-equals (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 1 t))

(defun rag/align-columns (begin end)
  "Align text columns"
  (interactive "r")
  ;; align-regexp syntax:  align-regexp (beg end regexp &optional group spacing repeat)
  (align-regexp begin end "\\(\\s-+\\)[a-zA-Z0-9=(),?':`\.{}]" 1 1 t)
  (indent-region begin end)) ; indent the region correctly after alignment

(defhydra align-hydra (:color blue)
  ("r" align-regexp "regexp")
  ("c" rag/align-columns "column")
  ("=" rag/align-equals "equals")
  ("w" rag/align-whitespace "whitespace")
  ("q" nil "quit"))

(defun rag/kill-rectangle-replace-with-space (start end)
  "Kill the rectangle and replace it with spaces."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end))
  (clear-rectangle start end)
  (setq deactivate-mark t)
  (if (called-interactively-p 'interactive)
      (indicate-copied-region (length (car killed-rectangle)))))

;; expand-region: expand region semantically
;; https://github.com/magnars/expand-region.el/tree/f99b7630efcdb47c9c6182489c55fba3bcaee521
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key "|")
  (setq expand-region-reset-fast-key "<ESC><ESC>"))

;; subword: subword movement and editing for camelCase
(use-package subword
  :ensure nil
  :defer 1
  :config (global-subword-mode))

;; save-place: save cursor position when buffer is killed
(use-package saveplace
  :ensure nil
  :defer 2
  :config (save-place-mode))



(defun xah-clean-empty-lines (&optional *begin *end *n)
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.

*N is the number of newline chars to use in replacement.
If 0, it means lines will be joined.
By befault, *N is 2. It means, 1 visible blank line.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-01-27"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (not *begin)
    (setq *begin (point-min) *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil "NOERROR")
          (replace-match (make-string (if *n *n 2) 10)))))))

(defun xah-clean-whitespace (&optional *begin *end)
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2016-10-15"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (null *begin)
    (setq *begin (point-min)  *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t]+\n" nil "noerror")
          (replace-match "\n")))
      (xah-clean-empty-lines (point-min) (point-max))
      (progn
        (goto-char (point-max))
        (while (equal (char-before) 32) ; char 32 is space
          (delete-char -1))))))



;;; Space Adjustment After Word Kills
(defun modi/just-one-space-post-kill-word (&rest _)
  "Function to manage white space after `kill-word' operations.
1. If point is at the beginning of the line after possibly some white space,
   remove that white space and re-indent that line.
2. If there is space before or after the point, ensure that there is only
   one white space around the point.
3. Otherwise, do nothing.
During the whole operation do not change the point position with respect to the
surrounding white space.
abc|   def  ghi <-- point on the left of white space after 'abc'
abc| ghi        <-- point still before white space after calling this function
abc   |def  ghi <-- point on the right of white space before 'def'
abc |ghi        <-- point still after white space after calling this function."
  (cond ((looking-back "^ *") ; remove extra space at beginning of line
         (save-excursion ; maintain the initial position of the pt w.r.t. space
           (just-one-space 0))
         (indent-according-to-mode))
        ((or (looking-at   " ")
             (looking-back " ")) ; adjust space only if it exists
         (save-excursion ; maintain the initial position of the pt w.r.t. space
           (just-one-space 1)))
        (t ; do nothing otherwise, includes the case where the point is at EOL
         )))
;; Delete extra horizontal white space after `kill-word' and `backward-kill-word'
(advice-add 'kill-word :after #'modi/just-one-space-post-kill-word)
(advice-add 'backward-kill-word :after #'modi/just-one-space-post-kill-word)



(defun xah-cycle-letter-case (arg)
  "Cycle the letter case of the selected region or the current word.
Cycles from 'lower' -> 'Capitalize' -> 'UPPER' -> 'lower' -> ..
        C-u M-x xah-cycle-letter-case -> Force convert to upper case.
    C-u C-u M-x xah-cycle-letter-case -> Force convert to lower case.
C-u C-u C-u M-x xah-cycle-letter-case -> Force capitalize."
  (interactive "p")
  (let (p1 p2
           (deactivate-mark nil)
           (case-fold-search nil))
    (if (use-region-p)
        (setq p1 (region-beginning)
              p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds)
              p2 (cdr bds))))

    (cl-case arg
      (4  (put this-command 'next-state "UPPER"))      ; Force convert to upper case
      (16 (put this-command 'next-state "lower"))      ; Force convert to lower case
      (64 (put this-command 'next-state "Capitalize")) ; Force capitalize
      (t (when (not (eq last-command this-command))
           (save-excursion
             (goto-char p1)
             (cond
              ;; lower -> Capitalize
              ((looking-at "[[:lower:]]")            (put this-command 'next-state "Capitalize"))
              ;; Capitalize -> UPPER
              ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'next-state "UPPER"))
              ;; Default: UPPER -> lower
              (t                                     (put this-command 'next-state "lower")))))))

    (cl-case (string-to-char (get this-command 'next-state)) ; `string-to-char' returns first character in string
      (?U (upcase-region p1 p2)
          ;; UPPER -> lower
          (put this-command 'next-state "lower"))
      (?l (downcase-region p1 p2)
          ;; lower -> Capitalize
          (put this-command 'next-state "Capitalize"))
      ;; Capitalization is a better Option here than upcasing the initials
      ;; because (upcase-initials "abc") -> "Abc" (good)
      ;;         (upcase-initials "ABC") -> "ABC" (not what I expect most of the times)
      ;;         (capitalize "abc")      -> "Abc" (good)
      ;;         (capitalize "ABC")      -> "Abc" (good)
      (t (capitalize-region p1 p2)
         ;; Capitalize -> UPPER
         (put this-command 'next-state "UPPER")))))

(defun modi/upcase ()     (interactive) (xah-cycle-letter-case 4))
(defun modi/downcase ()   (interactive) (xah-cycle-letter-case 16))
(defun modi/capitalize () (interactive) (xah-cycle-letter-case 64))

(bind-key "M-c" (defhydra hydra-change-case (:color blue
                                                     :hint nil)
"
_c_apitalize        _U_PCASE        _d_owncase        _<SPC>_ →Cap→UP→down→
"
                   ("c"     modi/capitalize)
                   ("U"     modi/upcase)
                   ("u"     modi/upcase)
                   ("d"     modi/downcase)
                   ("<SPC>" xah-cycle-letter-case :color red)
                   ("q"     nil "cancel" :color blue)))



(defvar yank-indent-modes '(js2-mode
                            emacs-lisp-mode
                            rust-mode
                            web-mode
                            css-mode
                            c++-mode
                            c-mode
                            racket-mode
                            typescript-mode
                            go-mode
                            sh-mode
                            shell-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 5000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text
(with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text
(with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))



;; Update The Timestamp Before saving a file
(add-hook 'before-save-hook #'time-stamp)

(bind-keys
 ("C-c o s" . cycle-spacing)
 ("M-;" . comment-line)
 ("C-c o o" . xah-clean-whitespace))

;; configuration for auto-fill-mode
(use-package simple :ensure nil
  :chords ((",m" . beginning-of-buffer)
           (",." . end-of-buffer))
  :hook ((prog-mode text-mode org-mode) . auto-fill-mode)
  :config
  (setq comment-auto-fill-only-comments t))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haskell-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun indent-defun (&optional l r)
  "Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun."
  (interactive)
  (let ((p (point-marker)))
    (set-marker-insertion-type p t)
    (indent-region
     (save-excursion
       (when l (goto-char l))
       (beginning-of-defun 1) (point))
     (save-excursion
       (when r (goto-char r))
       (end-of-defun 1) (point)))
    (goto-char p)))

(bind-keys
 ("C-M-\\" . indent-region-or-buffer)
 ("C-c d i" . indent-defun))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))
(bind-key "M-Q" 'unfill-paragraph)

(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun xah-title-case-region-or-line (*begin *end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (-p1
           -p2
           (-skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward -skipChars (line-beginning-position))
         (setq -p1 (point))
         (skip-chars-forward -skipChars (line-end-position))
         (setq -p2 (point)))
       (list -p1 -p2))))
  (let* ((-strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while
                 (search-forward (aref -x 0) nil t)
               (replace-match (aref -x 1) "FIXEDCASE" "LITERAL")))
           -strPairs))))))

;; move-text: move text or region up or down
;; https://github.com/emacsfodder/move-text
(use-package move-text
  :config (move-text-default-bindings))

;; undo-tree: Treat undo history as a tree
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :bind (("H-/" . undo-tree-redo))
  :init (global-undo-tree-mode))

;; utf-8 everywhere
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode)) ; pretty

;; smart-dash: underscores without having to press shift modifier for dash key
(use-package smart-dash
  :config (require 'smart-dash))

;; cycle-quotes: cycle between single and double quotes
(use-package cycle-quotes
  :bind ("C-c o q" . cycle-quotes))

;;; Eval and replace last sexp
;; http://stackoverflow.com/a/3035574/1219634
(defun eval-and-replace-last-sexp ()
  "Replace an emacs lisp expression (s-expression aka sexp) with its result.
How to use: Put the cursor at the end of an expression like (+ 1 2) and call
this command."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(bind-key "C-x C-S-e" #'eval-and-replace-last-sexp)

;;; Delete Blank Lines
;; http://www.masteringemacs.org/article/removing-blank-lines-buffer
;; If a region is selected, delete all blank lines in that region.
;; Else, call `delete-blank-lines'.
(defun modi/delete-blank-lines-in-region (&rest args)
  (let ((do-not-run-orig-fn (use-region-p)))
    (when do-not-run-orig-fn
      (flush-lines "^\\s-*$" (region-beginning) (region-end)))
    do-not-run-orig-fn))
(advice-add 'delete-blank-lines :before-until #'modi/delete-blank-lines-in-region)

(defadvice basic-save-buffer-2 (around fix-unwritable-save-with-sudo activate)
  "When we save a buffer which is write-protected, try to sudo-save it.

When the buffer is write-protected it is usually opened in
read-only mode.  Use \\[read-only-mode] to toggle
`read-only-mode', make your changes and \\[save-buffer] to save.
Emacs will warn you that the buffer is write-protected and asks
you to confirm if you really want to save.  If you answer yes,
Emacs will use sudo tramp method to save the file and then
reverts it, making it read-only again.  The buffer stays
associated with the original non-sudo filename."
           (condition-case err
               (progn
                 ad-do-it)
             (file-error
              (when (string-prefix-p
                     "Doing chmod: operation not permitted"
                     (error-message-string err))
                (let ((old-buffer-file-name buffer-file-name)
                      (success nil))
                  (unwind-protect
                      (progn
                        (setq buffer-file-name (concat "/sudo::" buffer-file-name))
                        (save-buffer)
                        (setq success t))
                    (setq buffer-file-name old-buffer-file-name)
                    (when success
                      (revert-buffer t t))))))))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
(bind-key "C-S-K" #'kill-back-to-indentation)

;; hungry-delete: deleting a whitespace character will delete all whitespace
;; until the next non-whitespace character
;; https://github.com/nflath/hungry-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode)
  (add-to-list 'hungry-delete-except-modes 'wdired-mode)
  (add-to-list 'hungry-delete-except-modes 'ivy-occur-mode))

;; poporg is a small Emacs Lisp project to help editing program strings and
;; comments using Org mode (or any other major mode).  This can be useful as it
;; is often more convenient to edit large pieces of text, like Emacs Lisp or
;; Python docstrings, in an org-mode buffer instead of in a comment or a string.
;; https://github.com/QBobWatson/poporg
(use-package poporg
  :bind (("C-c o p" . poporg-dwim)))

;; adaptive-wrap: Smart line-wrapping with wrap-prefix
;; https://github.com/emacsmirror/adaptive-wrap/blob/master/adaptive-wrap.el
(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config (setq-default adaptive-wrap-extra-indent 2))

(provide 'setup-editing)
