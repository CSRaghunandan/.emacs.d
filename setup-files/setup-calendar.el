;; Time-stamp: <2017-01-31 12:19:07 csraghunandan>

;;; calendar config

(defun sk/insert-date (prefix)
  "Insert the current date. With prefix-argument, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%A, %d %B %Y")
                 ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))
(bind-key "s-d" 'sk/insert-date)

(use-package calendar :defer t
  :config
  ;; Highlight today's date in the calendar
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(provide 'setup-calendar)
