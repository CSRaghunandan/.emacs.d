;; Time-stamp: <2017-01-08 20:46:30 csraghunandan>

;; ace-window
;; https://github.com/abo-abo/ace-window
;; allows for quick switching of windows within the current emacs frame
(use-package ace-window
  :commands (ace-window)
  :bind* ("C-c w" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; display the ace-window key to switch to in mode-line
  (ace-window-display-mode 1)
  ;; enable aw dispatch even for just one window
  (setq aw-dispatch-always t)

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
    ("n" scroll-other-window "scroll")
    ("p" scroll-other-window-down "scroll down")
    ("q" nil :color blue))

  ;; add hydras to control window size and scroll other window
  (setq aw-dispatch-alist
    '((?x aw-delete-window " Ace - Delete Window")
      (?m aw-swap-window " Ace - Swap Window")
      (?n aw-flip-window " Ace - Flip Window")
      (?v aw-split-window-vert " Ace - Split Vert Window")
      (?b aw-split-window-horz " Ace - Split Horz Window")
      (?o delete-other-windows " Ace - Maximize Window")
      (?c hydra-window-scroll/body)
      (?y hydra-window-size/body))))

(provide 'setup-ace-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'

;; ace-window dispatch operations:
;;
;; x - delete window
;; m - swap (move) window
;; v - split window vertically
;; b - split window horizontally
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window

