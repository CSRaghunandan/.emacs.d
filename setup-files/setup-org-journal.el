;; Time-stamp: <2016-10-24 18:47:30 csraghunandan>

;; A journaling tool with org-mode: `org-journal'
;; https://github.com/bastibe/org-journal

(use-package org-journal)

(provide 'setup-org-journal)
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
