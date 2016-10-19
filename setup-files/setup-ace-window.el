;; Time-stamp: <2016-10-19 19:30:01 csraghunandan>

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
;; x - delete window
;; m - swap (move) window
;; v - split window vertically
;; b - split window horizontally
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window

