;;; setup-search.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-10 13:15:18 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; search config

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

(provide 'setup-search)
