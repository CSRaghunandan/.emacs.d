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

(defun modi/isearch-backward-symbol-at-point ()
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

(bind-keys*
 ("C-S-s" . isearch-forward-symbol-at-point)
 ("C-S-r" . modi/isearch-backward-symbol-at-point))

(use-package visual-regexp-steroids
  :bind* (("C-c q" . vr/query-replace)
          ("C-c M" . vr/mc-mark))
  :config
  (setq vr/default-feedback-limit 300))

(provide 'setup-search)
