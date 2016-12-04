;; Time-stamp: <2016-12-04 23:05:23 csraghunandan>

;; Theme configuration for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :config
  ;; make fringe blends into the background
  (set-face-attribute 'fringe nil :background "gray27")

  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; original `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg-05))))))

  (custom-set-faces
   ;; make function name face brighter so it's easily distinguishable from
   ;; font-lock face for types
   '(font-lock-function-name-face ((t (:foreground "#A5FBFF"))))
   ;; set gray colours for comments
   '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
   '(font-lock-comment-face ((t (:foreground "gray55" :slant italic))))
   ;; make links appear gray as well
   '(link ((t (:foreground "gray55" :underline t :weight normal))))
   ;; fontify org headlines. Make them slightly bigger
   '(org-document-title ((t (:height 1.3))))
   '(org-level-1 ((t (:inherit outline-1 :foreground "#DFAF8F" :height 1.2))))
   '(org-level-2 ((t (:inherit outline-2 :foreground "#BFEBBF" :height 1.15))))
   '(org-level-3 ((t (:inherit outline-3 :foreground "#7CB8BB" :height 1.1))))))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; Better defaults for emacs
(column-number-mode t)
(line-number-mode t)
(tool-bar-mode -1)
;; if PragmataPro font is available, use it
(when (member "PragmataPro" (font-family-list))
  (set-frame-font "PragmataPro 13"))
(setq ring-bell-function 'ignore)
;; resize minibuffer window to accommodate text
(setq resize-mini-window t)
;; don't show splash screen when starting emacs
(setq inhibit-splash-screen t)

;; cursor settings
(setq-default cursor-type '(bar . 1))
(blink-cursor-mode -1)

;; disable the ugly scrollbars
(scroll-bar-mode -1)
(fringe-mode '(8 . 8))
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

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
