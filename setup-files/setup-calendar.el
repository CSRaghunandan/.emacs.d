;; Time-stamp: <2018-05-04 00:31:56 csraghunandan>

;; calendar config
(use-package calendar :defer t
  :ensure nil
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
