;;; setup-rg -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-22 23:12:59 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; fast, friendly searching with ripgrep and Emacs
;; https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :if (executable-find "rg")
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

(provide 'setup-rg)
