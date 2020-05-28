;;; setup-window.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-28 16:23:51 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; This program provides some interactive functions which allows users
;; to transpose windows arrangement in currently selected frame
;; https://github.com/emacsorphanage/transpose-frame/blob/master/transpose-frame.el
(use-package transpose-frame
  :bind ("C-c h w" . hydra-transpose-window/body)
  :config

  (defhydra hydra-transpose-window (:color red :columns 3)
    "Transpose window arrangement"
    ("t" transpose-frame "transpose frame")
    ("v" flip-frame "flip vertically")
    ("h" flop-frame "flip horizontally")
    ("r" rotate-frame "Rotate frame 180 degrees")
    ("c" rotate-frame-clockwise "Rotate 90 degrees clockwise")
    ("a" rotate-frame-anticlockwise "Rotate 90 degrees anti clockwise")
    ("q" nil "Quit" :color blue)))

;; ace-window: quick switching of windows
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :commands (ace-window)
  :bind* ("C-c w" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; ignore case
  (setq aw-translate-char-function #'downcase)
  ;; enable aw dispatch even for just one window
  (setq aw-dispatch-always t)

  ;; enable the ace-window number in mode-line
  (ace-window-display-mode)

  ;; add hydras to control window size and scroll other window
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Window")
          (?c aw-copy-window "Copy Window")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?o delete-other-windows "Maximize Window")
          (?e aw-execute-command-other-window "Execute Command Other Window")
          (?v hydra-window-scroll/body)
          (?t aw-split-window-fair "split fairly")
          (?2 aw-split-window-vert "split vertically")
          (?3 aw-split-window-horz "split horizontally")
          (?y hydra-window-size/body)
          (?? aw-show-dispatch-help)))

  (defhydra hydra-window-size (:color red :columns 3)
    "Windows size"
    ("b" shrink-window-horizontally "shrink horizontal")
    ("p" shrink-window "shrink vertical")
    ("n" enlarge-window "enlarge vertical")
    ("f" enlarge-window-horizontally "enlarge horizontal")
    ("m" toggle-frame-fullscreen "maximize frame")
    ("r" balance-windows "balance windows")
    ("q" nil "Quit" :color blue))

  (defhydra hydra-window-scroll (:color red)
    "Scroll other window"
    ("<SPC>" scroll-other-window "scroll")
    ("b" scroll-other-window-down "scroll down")
    ("q" nil "Quit" :color blue)))

;; A simple-minded way of managing window configs in emacs
;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-mode-line-style nil))

(provide 'setup-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'

;; ace-window dispatch operations:
;;
;; x - delete window
;; m - swap window
;; c - copy window
;; n - flip window (select previous window)
;; u - switch buffer other window
;; j - switch buffer in window
;; o - maximize current window
;; e - execute command other window
;; v - Hydra scroll
;; t - split window fairly
;; 2 - split vertically
;; 3 - split horizontally
;; y - Hydra window size
;; ? - show help for ace-window
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window

