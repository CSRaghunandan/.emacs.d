;;; setup-optimizations.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-10 17:51:17 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;;; optimizations to improve performance
;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024))

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Horizontally scroll only the current line
;; https://www.reddit.com/r/emacs/comments/6au45k/is_it_possible_to_truncate_long_lines_the_same/
(setq auto-hscroll-mode 'current-line)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; GCMH - the Garbage Collector Magic Hack
;; https://gitlab.com/koral/gcmh
(use-package gcmh
  :config
  (setq gcmh-idle-delay 10 ; garbage collect after 10s of idle time
        gcmh-high-cons-threshold 16777216) ; 16mb
  )

(provide 'setup-optimizations)
