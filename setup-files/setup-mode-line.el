;; Time-stamp: <2018-06-22 03:21:47 csraghunandan>

;; flash the modeline instead of ringing the bell
;; https://github.com/purcell/mode-line-bell
(use-package mode-line-bell
  :defer 1
  :config (mode-line-bell-mode))

(defvar mu-eyebrowse-mode-line
  '(:propertize
    (:eval
     (when (bound-and-true-p eyebrowse-mode)
       (let* ((num (eyebrowse--get 'current-slot))
              (tag (when num
                     (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
              (str (concat
                    " "
                    (if (and tag (< 0 (length tag)))
                        tag
                      (when num
                        (int-to-string num)))
                    " ")))
         str)))
    face (:background "#81a2be" :foreground "#373b41"))
  "Mode line format for Eyebrowse.")

(put 'mu-eyebrowse-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mu-eyebrowse-mode-line
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-buffer-identification " " mode-line-position
                (vc-mode vc-mode)
                (multiple-cursors-mode mc/mode-line)
                " " mode-line-modes
                " " mode-line-misc-info
                mode-line-end-spaces))

;; Tabs and ribbons for the mode-line
;; https://github.com/tarsius/moody
(use-package moody
  :config
  ;; only use apple-rgb for MacOS
  (when (is-mac-p)
    (setq moody-slant-function #'moody-slant-apple-rgb))
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 22)

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (column-number-mode)
  (size-indication-mode)

  ;; display date and time
  (setq display-time-format "%a-%d %H:%M")
  (setq display-time-default-load-average nil)
  (display-time-mode))

;; A minor-mode menu for the mode line
;; https://github.com/tarsius/minions
(use-package minions
  :init (minions-mode)
  :config
  (setq minions-mode-line-lighter "#")
  (setq minions-direct '(flycheck-mode)))

;; macro to rename mode-name for major-modes
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "typescript-mode" typescript-mode "TS")
(rename-modeline "haskell-mode" haskell-mode "𝞴=")

(provide 'setup-mode-line)

;; Variables used in display-time-format
;; http://docs.splunk.com/Documentation/Splunk/5.0.2/SearchReference/Commontimeformatvariables
;;
;; | %y | year in numbers (2-digit)                   |
;; | %Y | year in numbers (4-digit)                   |
;; | %m | month in number (eg: 12)                    |
;; | %B | full month name (eg: December)              |
;; | %b | short month name (eg: Dec)                  |
;; | %d | day in numbers, with leading zeros (eg: 08) |
;; | %e | day in numbers, no leading zeros (eg: 8)    |
;; | %A | full weekday name (eg: Sunday)              |
;; | %a | short weekday name (eg: Sun)                |
;; | %H | hours in 24-clock, with leading zeros       |
;; | %k | hours in 24-clock, no leading zeros         |
;; | %l | hours in 12-clock, with leading zeros       |
;; | %p | am/pm                                       |
;; | %T | time in 24-hour notation (%H:%M:%S)         |
