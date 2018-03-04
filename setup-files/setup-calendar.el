;; Time-stamp: <2018-03-04 20:48:51 csraghunandan>

;; calendar config
(use-package calendar :defer t
  :hook ((calendar-today-visible . calendar-mark-today))
  :bind (("s-d" . sk/insert-date))
  :config
  (defun sk/insert-date (prefix)
    "Insert the current date. With prefix-argument, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%A, %d %B %Y")
                   ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
      (insert (format-time-string format)))))

(provide 'setup-calendar)
