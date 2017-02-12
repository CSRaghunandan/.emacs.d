;; Time-stamp: <2017-02-12 13:23:52 csraghunandan>

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
(use-package visual-regexp)

;; visual-regexp-steroids: use modern regexp engines instead of emacs one.
;; https://github.com/benma/visual-regexp-steroids.el/
(use-package visual-regexp-steroids
  :bind (("C-c q" . vr/query-replace)
          ("C-c v m" . vr/mc-mark))
  :config (setq vr/default-feedback-limit 300))

;; wgrep: Writable grep buffer and apply the changes to files
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep)

;;; anzu: show number of searches in isearch
;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :diminish anzu-mode
  :config
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-to-string-separator " => ")
  (global-set-key [remap query-replace] 'anzu-query-replace)
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

(provide 'setup-search)
