;;; setup-search.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-24 12:40:42 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; isearch config

;; ignore cases while searching
(setq-default case-fold-search t)
;; Isearch convenience, space matches anything (non-greedy) - Fuzzy search
(setq search-whitespace-regexp ".*?")

;; Allow scrolling while isearch is active
;; Example: C-s foo C-l (to recenter the point in buffer to center/top/bottom)
;; http://emacs.stackexchange.com/a/10313/115
(setq isearch-allow-scroll t)

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)
;; The beauty of scrolling while searching is that the current match never goes
;; off-screen. So you can even use C-v/M-v without worrying that you'll lose
;; the current match location.

(defun rag/isearch-backward-symbol-at-point ()
  "Do incremental search backward for a symbol found near point.
Like ordinary incremental search except that the symbol found at point
is added to the search string initially as a regexp surrounded
by symbol boundary constructs \\_< and \\_>.
See the command `isearch-forward-symbol' for more information."
  (interactive)
  (isearch-mode (not :forward) nil nil nil 'isearch-symbol-regexp)
  (let ((bounds (find-tag-default-bounds)))
    (cond
     (bounds
      (when (< (car bounds) (point))
        (goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No symbol at point")
      (isearch-update)))))

(bind-keys
 ("s-f" . isearch-forward-symbol-at-point)
 ("s-r" . rag/isearch-backward-symbol-at-point))

(bind-key "C-'" 'avy-isearch isearch-mode-map)

;; visual-regexp: A regexp/replace command for Emacs with interactive visual feedback
;; https://github.com/benma/visual-regexp.el
(use-package visual-regexp
  :defer t)

;; visual-regexp-steroids: use modern regexp engines instead of emacs one.
;; https://github.com/benma/visual-regexp-steroids.el/
(use-package visual-regexp-steroids
  :bind (("C-c q" . vr/query-replace)
          ("C-c v m" . vr/mc-mark))
  :config (setq vr/default-feedback-limit 300))

;; wgrep: Writable grep buffer and apply the changes to files
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :config (setq wgrep-auto-save-buffer t))

;;; anzu: show number of searches in isearch
;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :defer t
  :config
  (setq anzu-search-threshold 1000)
  (global-anzu-mode +1))

;;; Query exchange
;; Inspired from http://www.emacswiki.org/emacs/QueryExchange and definition of
;; `query-replace-regexp' from replace.el
(defun query-exchange (string-1 string-2 &optional delimited start end)
  "Exchange string-1 and string-2 interactively.
The user is prompted at each instance like query-replace. Exchanging
happens within a region if one is selected."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " regexp"
                   (if (and transient-mark-mode mark-active) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (perform-replace
   (concat "\\(" string-1 "\\)\\|" string-2)
   '(replace-eval-replacement replace-quote
                              (if (match-string 1) string-2 string-1))
   t t delimited nil nil start end))

;; A helpful query-replace for Emacs
;; https://github.com/Wilfred/ez-query-replace.el
(use-package ez-query-replace
  :bind (([remap query-replace] . ez-query-replace)
         ("C-c M-%" . ez-query-replace-repeat)))

;; fast, friendly searching with ripgrep and Emacs
;; https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :bind (("C-c d g" . deadgrep))
  :config
  (setq-default deadgrep--search-type 'regexp) ;Default is 'string

  ;; https://github.com/kaushalmodi/.emacs.d/blob/a03fadccb8a19a037045a1b2964235185e5dd085/setup-files/setup-rg.el
  (defun modi/deadgrep--jump-to-and-execute (re)
    "Execute the button that matches RE and push it."
    (goto-char (point-min))
    (re-search-forward re)
    (backward-char 3)
    (push-button))

  (defun modi/deadgrep-change-search-term ()
    "Change the search term."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Search term: .*change$"))

  (defun modi/deadgrep-change-search-type-to-string ()
    "Change the search type to 'string'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Search type: .*string"))

  (defun modi/deadgrep-change-search-type-to-words ()
    "Change the search type to 'words'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Search type: .*words"))

  (defun modi/deadgrep-change-search-type-to-regexp ()
    "Change the search type to 'regexp'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Search type: .*regexp"))

  (defun modi/deadgrep-change-case-to-smart ()
    "Change the case sensitivity to 'smart'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Case: .*smart"))

  (defun modi/deadgrep-change-case-to-sensitive ()
    "Change the case sensitivity to 'sensitive'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Case: .*sensitive"))

  (defun modi/deadgrep-change-case-to-ignore ()
    "Change the case sensitivity to 'ignore'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Case: .*ignore"))

  (defun modi/deadgrep-change-context-to-none ()
    "Don't show ny context around the search results."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Context: .*none"))

  (defun modi/deadgrep-change-context-to-before ()
    "Set 'before' context for the search results."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Context: .*before"))

  (defun modi/deadgrep-change-context-to-after ()
    "Set 'after' context for the search results."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Context: .*after"))

  (defun modi/deadgrep-change-directory ()
    "Change the root directory for searches."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Directory: .*$"))

  (defun modi/deadgrep-search-all-files ()
    "Change file search scope to 'all'."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Files: .*all"))

  (defun modi/deadgrep-search-files-by-type ()
    "Search only in the specified file types."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Files: .*type"))

  (defun modi/deadgrep-search-files-by-glob ()
    "Search in files names that match the specified glob."
    (interactive)
    (modi/deadgrep--jump-to-and-execute "^Files: .*glob"))

  (bind-keys
   :map deadgrep-mode-map
   ("s" . modi/deadgrep-change-search-term)
   ("ts" . modi/deadgrep-change-search-type-to-string)
   ("tw" . modi/deadgrep-change-search-type-to-words)
   ("tr" . modi/deadgrep-change-search-type-to-regexp)
   ("cs" . modi/deadgrep-change-case-to-smart)
   ("cc" . modi/deadgrep-change-case-to-sensitive)
   ("ci" . modi/deadgrep-change-case-to-ignore)
   ("xn" . modi/deadgrep-change-context-to-none)
   ("xb" . modi/deadgrep-change-context-to-before)
   ("xa" . modi/deadgrep-change-context-to-after)
   ("d" . modi/deadgrep-change-directory)
   ("fa" . modi/deadgrep-search-all-files)
   ("ft" . modi/deadgrep-search-files-by-type)
   ("fg" . modi/deadgrep-search-files-by-glob)))

(provide 'setup-search)
