;;; setup-ace-window.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-13 16:56:19 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

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
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?o delete-other-windows "Maximize Window")
          (?e aw-execute-command-other-window "Execute Command Other Window")
          (?T aw-transpose-frame "Transpose Frame")
          (?v hydra-window-scroll/body)
          (?t aw-split-window-fair "split fairly")
          (?2 aw-split-window-vert "split vertically")
          (?3 aw-split-window-horz "split horizontally")
          (?y hydra-window-size/body)
          (?? aw-show-dispatch-help)))

  (defhydra hydra-window-size (:color red)
    "Windows size"
    ("b" shrink-window-horizontally "shrink horizontal")
    ("p" shrink-window "shrink vertical")
    ("n" enlarge-window "enlarge vertical")
    ("f" enlarge-window-horizontally "enlarge horizontal")
    ("m" toggle-frame-fullscreen "maximize frame")
    ("r" balance-windows "balance windows")
    ("q" nil :color blue))

  (defhydra hydra-window-scroll (:color red)
    "Scroll other window"
    ("<SPC>" scroll-other-window "scroll")
    ("b" scroll-other-window-down "scroll down")
    ("q" nil :color blue)))

(provide 'setup-ace-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'

;; ace-window dispatch operations:
;;
;; x - delete window
;; m - swap window
;; M - move window
;; c - copy window
;; n - flip window (select previous window)
;; u - switch buffer other window
;; j - switch buffer in window
;; o - maximize current window
;; e - execute command other window
;; T - transpose frame
;; v - Hydra scroll
;; t - split window fairly
;; y - Hydra window size
;; ? - show help for ace-window
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window

