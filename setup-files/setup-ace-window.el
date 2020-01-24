;;; setup-ace-window.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-10-16 16:39:24 csraghunandan>

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

  ;; add hydras to control window size and scroll other window
  (setq aw-dispatch-alist
   '((?x aw-delete-window " Ace - Delete Window")
     (?m aw-swap-window " Ace - Swap Window")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?j aw-switch-buffer-in-window "Select Buffer")
     (?o delete-other-windows " Ace - Maximize Window")
     (?c hydra-window-scroll/body)
     (?t aw-split-window-fair "Ace- split fairly")
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
;; m - swap (move) window
;; t - split window fairly
;; u - switch buffer other window
;; j - switch buffer in window
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window
;; ? - show help for ace-window
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window

