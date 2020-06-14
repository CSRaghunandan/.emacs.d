;;; setup-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-15 00:36:46 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
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

;; require a final new line
(setq require-final-newline t)

;;;; Pull Up Line
;; http://emacs.stackexchange.com/q/7519/115
(defun modi/pull-up-line ()
  "Join the following line onto the current one.
This is analogous to \\[move-end-of-line] followed by
\\[delete-foward], or \\[universal-argument] \\[delete-indentation],
or \\[universal-argument] \\[join-line].
If the current line is a comment and the pulled-up line is also a
comment, remove the leading comment characters from that line."
  (interactive)
  (join-line -1)
  (when (nth 4 (syntax-ppss))           ;If the current line is a comment
    ;; Remove comment prefix chars from the pulled-up line if present.
    (save-excursion
      ;; Delete all comment-start and space characters, one at a time.
      (while (looking-at (concat "\\s<"  ;Comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ;First char of `comment-start'
                                 "\\|" "[[:blank:]]"))               ;Extra spaces
        (delete-forward-char 1))
      (insert-char ? ))))               ;Insert space

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
 ("M-j" . modi/pull-up-line)
 ("H-j" . rag/push-up-line))

;;;; Open Line
(defun modi/smart-open-line (&optional n)
  "Move the current line down if there are no word chars between the start of
line and the cursor. Else, insert empty line after the current line."
  (interactive "p")
  (if (derived-mode-p 'org-mode)
      (dotimes (cnt n)
        (org-open-line 1))
    ;; Get the substring from start of line to current cursor position
    (let ((str-before-point (buffer-substring (line-beginning-position) (point))))
      ;; (message "%s" str-before-point)
      (if (not (string-match "\\w" str-before-point))
          (progn
            (dotimes (cnt n)
              (newline-and-indent))
            ;; (open-line 1)
            (previous-line n)
            (indent-relative-maybe))
        (progn
          (move-end-of-line nil)
          (dotimes (cnt n)
            (newline-and-indent))
          (previous-line (- n 1)))))))

(defun rag/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(bind-keys
 ("C-o" . modi/smart-open-line)
 ("C-S-o" . rag/smart-open-line-above))



;; https://github.com/leoliu/easy-kill
;; Kill & Mark Things Easily in Emacs
(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))



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

;; undo-tree: tree like navigation for undo/redo in emacs
;; article about undo-tree bugs: http://www.dr-qubit.org/Lost_undo-tree_history.html
(use-package undo-tree
  :config (global-undo-tree-mode 1))

;; subword: subword movement and editing for camelCase
(use-package subword
  :straight nil
  :defer 1
  :config (global-subword-mode))

;; save-place: save cursor position when buffer is killed
(use-package saveplace
  :straight nil
  :defer 2
  :config (save-place-mode))


(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "white space cleaned"))))



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



;;; Cycle Letter Case
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
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
      ;; Capitalization is a better option here than upcasing the initials
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
                            rustic-mode
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
(use-package simple :straight nil
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
      (flush-lines "^[[:blank:]]*$" (region-beginning) (region-end)))
    do-not-run-orig-fn))
(advice-add 'delete-blank-lines :before-until #'modi/delete-blank-lines-in-region)

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
  (add-to-list 'hungry-delete-except-modes 'dired-mode)
  (add-to-list 'hungry-delete-except-modes 'vterm-mode)
  (add-to-list 'hungry-delete-except-modes 'ivy-occur-mode))

;; poporg is a small Emacs Lisp project to help editing program strings and
;; comments using Org mode (or any other major mode).  This can be useful as it
;; is often more convenient to edit large pieces of text, like Emacs Lisp or
;; Python docstrings, in an org-mode buffer instead of in a comment or a string.
;; https://github.com/QBobWatson/poporg
(use-package poporg
  :bind (("C-c o c" . poporg-dwim)))

;; duplicate-thing.el is Emacs lisp. Easy duplicate line or region, with comment out.
;; https://github.com/ongaeshi/duplicate-thing
(use-package duplicate-thing
  :bind (("C-c d t" . duplicate-thing)))

;; https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))

(bind-keys ([remap keyboard-quit] . keyboard-quit-context+))

;; editorconfig integration for emacs
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig :defer 2
  :config
  (editorconfig-mode 1))

;; Preserve the scratch buffer across Emacs sessions
;; https://github.com/Fanael/persistent-scratch
(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

;; sudo-edit: Utilities for opening files with sudo
;; https://github.com/nflath/sudo-edit
(use-package sudo-edit
  :bind
  ("C-c s u" . sudo-edit)
  :config
  (sudo-edit-indicator-mode)
  ;; set a less intrusive header for sudo-edit-indicator-mode
  (set-face-attribute 'sudo-edit-header-face nil :background "#2E3440"))

(provide 'setup-editing)
