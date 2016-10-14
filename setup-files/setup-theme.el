;; Time-stamp: <2016-10-14 12:19:39 csraghunandan>

;; Theme configuration for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme)

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; Better defaults for emacs
(column-number-mode t)
(line-number-mode t)
(tool-bar-mode -1)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)
(set-frame-font "PragmataPro 13")
(setq ring-bell-function 'ignore)
;; resize minibuffer window to accommodate text
(setq resize-mini-window t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message ";; Let the games begin.\n\n")
(setq-default cursor-type '(bar . 1))
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(8 . 8))
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; Remove background for fringe
(set-face-attribute 'fringe nil :background "gray21")
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box 'nil)
;; prevent cursor from moving when scrolling
(setq scroll-preserve-screen-position t)

;; set continuation indicators to right fringe only
(setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      '(nil right-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; make emacs start with full screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(provide 'setup-theme)
