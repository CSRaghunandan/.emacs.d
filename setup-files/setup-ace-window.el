;; Time-stamp: <2016-10-16 00:05:01 csraghunandan>

;; ace-window
;; https://github.com/abo-abo/ace-window
;; allows for quick switching of windows within the current emacs frame
(use-package ace-window
  :commands (ace-window)
  :bind* ("C-c w" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; display the ace-window key to switch to in mode-line
  (ace-window-display-mode 1))

(provide 'setup-ace-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'

;; ace-window dispatch operations:
;;
;; | x | aw-delete-window     | Ace - Delete Window     |
;; | m | aw-swap-window       | Ace - Swap Window       |
;; | n | aw-flip-window       |                         |
;; | v | aw-split-window-vert | Ace - Split Vert Window |
;; | b | aw-split-window-horz | Ace - Split Horz Window |
;; | i | delete-other-windows | Ace - Maximize Window   |
;; | o | delete-other-windows |                         |
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window
