;; Time-stamp: <2018-03-13 03:04:46 csraghunandan>

;; flash the modeline instead of ringing the bell
;; https://github.com/purcell/mode-line-bell
(use-package mode-line-bell
  :defer 1
  :config (mode-line-bell-mode))

(defvar mu-eyebrowse-mode-line
  '(:propertize
    (:eval
     (when (and (bound-and-true-p eyebrowse-mode)
                (< 1 (length (eyebrowse--get 'window-configs))))
       (let* ((num (eyebrowse--get 'current-slot))
              (tag (when num
                     (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
              (str (concat
                    " "
                    (if (and tag (< 0 (length tag)))
                        tag
                      (when num (int-to-string num)))
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
  (setq moody-slant-function #'moody-slant-apple-rgb)
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 22)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (column-number-mode))

;; A minor-mode menu for the mode line
;; https://github.com/tarsius/minions
(use-package minions
  :init (minions-mode)
  :config (setq minions-direct '(flycheck-mode)))

;; macro to rename mode-name for major-modes
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "typescript-mode" typescript-mode "TS")
(rename-modeline "haskell-mode" haskell-mode "𝞴=")

(let ((line (face-attribute 'mode-line :underline)))
  (zenburn-with-color-variables

    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line          nil :background "gray30")
    (set-face-attribute 'mode-line-inactive nil :background zenburn-bg-1)
    (set-face-attribute 'mode-line          nil :foreground zenburn-green+4)
    (set-face-attribute 'mode-line-inactive nil :foreground "gray70")))

(provide 'setup-mode-line)
