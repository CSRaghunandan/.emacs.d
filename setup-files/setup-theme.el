;; Time-stamp: <2016-12-09 12:39:12 csraghunandan>

;; Theme configuration for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :config

  ;; make zenburn theme look darker by default
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg-05))))))

  ;; make function face brighter so it's easily distinguishable from font-lock face for types
  (set-face-attribute 'font-lock-function-name-face nil :foreground "cadetblue1")
  ;; make everything look gray :)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "gray55")
  (set-face-attribute 'font-lock-comment-face nil :foreground "gray55" :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray70" :slant 'italic)
  (set-face-attribute 'link nil :foreground "gray55" :underline t :weight 'normal)
  (set-face-background 'shm-current-face "#444444")
  (set-face-attribute 'hl-line nil :background "#444444")
  (set-face-attribute 'fringe nil :background "#444444")
  (set-face-attribute 'vhl/default-face nil :background "#444444")
  (set-face-foreground 'highlight-indent-guides-character-face "gray40")
  ;; make mode line look pretty :)
  (set-face-attribute 'mode-line nil :box nil :weight 'bold :foreground "gray60")
  (set-face-attribute 'mode-line-inactive nil :box nil :weight 'bold :foreground "gray60")
  (set-face-attribute 'powerline-active2 nil :background "#554A4A")
  (set-face-attribute 'powerline-active1 nil :background "#554A4A" :foreground "#F0DFAF")
  (set-face-attribute 'mode-line-buffer-id nil :foreground "#F0DFAF")
  ;; dim inactive modeline
  (set-face-attribute 'powerline-inactive2 nil :background "gray28")
  (set-face-attribute 'powerline-inactive1 nil :background "gray28"))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Better defaults for emacs
(column-number-mode t)
(line-number-mode t)
;; disable the ugly toolbar
(tool-bar-mode -1)
;; if PragmataPro font is available, use it
(when (member "PragmataPro" (font-family-list))
  (set-frame-font "PragmataPro 13"))
(setq ring-bell-function 'ignore)
;; resize minibuffer window to accommodate text
(setq resize-mini-window t)
;; don't show splash screen when starting emacs
(setq inhibit-splash-screen t)
;; don't echo startup message of GNU emacs
(setq inhibit-startup-echo-area-message t)

;; cursor settings
(setq-default cursor-type '(bar . 1))
;; disable annoying cursor blinks
(blink-cursor-mode -1)

;; disable the ugly scrollbars
(scroll-bar-mode -1)
(fringe-mode '(8 . 8))
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

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
