;;; setup-calendar.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-04 22:06:41 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; calendar config
(use-package calendar
  :straight nil
  :hook ((calendar-today-visible . calendar-mark-today))
  :bind (("H-d" . help/hydra/timestamp/body))
  :config
  (defhydra help/hydra/timestamp (:color blue :hint nil)
    "
                                                         ╭────────────┐
                                                         │ Timestamps │
 ╭───────────────────────────────────────────────────────┴────────────╯
                                                         Quit [_q_]
  ISO         : date [_d_]
  U.S.        : date [_/_]  year   [_y_] dashes [_-_]  words [_w_]
  Stamp       : UTC  [_s_]  human  [_h_]
  Epoch       : unix [_u_]  J2000  [_m_]
  Org Active  : date [_a_]  pick   [_p_]
  Org Inactive: date [_i_]  choose [_c_]
"
    ("d" help/insert-datestamp)                         ; 2018-05-12
    ("/" help/insert-datestamp-us)                      ; 05/12/18
    ("y" help/insert-datestamp-us-full-year)            ; 05/12/2018
    ("-" help/insert-datestamp-us-full-year-and-dashes) ; 05-12-2018
    ("w" help/insert-datestamp-us-words)                ; Saturday May 12, 2018

    ("s" help/insert-timestamp)                         ; 2018-05-12T11:50:01+07:00
    ("h" help/insert-human-stamp) ; 2018-05-12T12:36@Prek Leap (11.642600N-104.919210W)

    ("u" help/insert-unix-time)                         ; 1526100829
    ("m" insert-epoch)                                  ; 579372688.143557080

    ("a" help/org-time-stamp-with-seconds-now)          ; <2018-05-12 Sat 11:50>
    ("p" org-time-stamp)
    ("i" help/org-time-stamp-inactive-with-seconds-now) ; [2018-05-12 Sat 11:50]
    ("c" org-time-stamp-inactive)
    ("q" nil :color blue))

  (defun help/insert-datestamp ()
    "Produces and inserts a partial ISO 8601 format timestamp."
    (interactive)
    (insert (format-time-string "%F")))

  (defun help/insert-datestamp-us ()
    "Produces and inserts a US datestamp."
    (interactive)
    (insert (format-time-string "%m/%d/%y")))

  (defun help/insert-datestamp-us-full-year-and-dashes ()
    "Produces and inserts a US datestamp with full year and dashes."
    (interactive)
    (insert (format-time-string "%m-%d-%Y")))

  (defun help/insert-datestamp-us-full-year ()
    "Produces and inserts a US datestamp with full year."
    (interactive)
    (insert (format-time-string "%m/%d/%Y")))

  (defun help/insert-datestamp-us-words ()
    "Produces and inserts a US datestamp using words."
    (interactive)
    (insert (format-time-string "%A %B %d, %Y")))

  (defun help/insert-timestamp-no-colons ()
    "Inserts a full ISO 8601 format timestamp with colons replaced by hyphens."
    (interactive)
    (insert (help/get-timestamp-no-colons)))

  (defun help/insert-datestamp ()
    "Produces and inserts a partial ISO 8601 format timestamp."
    (interactive)
    (insert (format-time-string "%F")))

  (defun help/insert-unix-time ()
    "Produce seconds since 1970-01-01 00:00:00 UTC"
    (interactive)
    (insert (format-time-string "%s")))

  (defun help/get-timestamp ()
    "Produces a full ISO 8601 format timestamp."
    (interactive)
    (let* ((timestamp-without-timezone (format-time-string "%Y-%m-%dT%T"))
           (timezone-name-in-numeric-form (format-time-string "%z"))
           (timezone-utf-offset
            (concat (substring timezone-name-in-numeric-form 0 3)
                    ":"
                    (substring timezone-name-in-numeric-form 3 5)))
           (timestamp (concat timestamp-without-timezone
                              timezone-utf-offset)))
      timestamp))

  (defun help/insert-timestamp ()
    "Inserts a full ISO 8601 format timestamp."
    (interactive)
    (insert (help/get-timestamp)))

  (defun help/org-time-stamp-with-seconds-now ()
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-time-stamp)))

  (defun help/org-time-stamp-inactive-with-seconds-now ()
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-time-stamp-inactive)))

  (defun insert-epoch ()
    "Insert time in seconds from the J2000.0 epoch in
   sec.microseconds, by subtracting unix-time"
    (interactive)
    (insert (epoch-J2000)))

  (defun epoch-J2000 ()
    "Get time in seconds from the J2000.0 epoch in
   sec.microseconds, by subtracting seconds unix-time
   until year 2000"
    (replace-regexp-in-string "\n\\'" ""
			                  (concat
			                   (number-to-string
			                    (round (- (string-to-number
					                       (shell-command-to-string "date +%s")) 946727935)))
			                   (shell-command-to-string "date +.%N")))))

(provide 'setup-calendar)
