;;; setup-visual.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-02-13 23:38:34 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; https://github.com/Fanael/rainbow-delimiters
;; different colours for each nested delimiter
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; better defaults
;; don't show splash screen when starting emacs
(setq inhibit-splash-screen t)
;; don't echo startup message of GNU emacs
(setq inhibit-startup-echo-area-message t)

;; Supply a random fortune cookie as the *scratch* message.
(defun my-fortune-scratch-message ()
  (interactive)
  (let ((fortune
         (when (executable-find "fortune")
           (with-temp-buffer
             (shell-command "fortune" t)
             (let ((comment-start ";;")
                   (comment-empty-lines t)
                   (tab-width 4))
               (untabify (point-min) (point-max))
               (comment-region (point-min) (point-max)))
             (delete-trailing-whitespace (point-min) (point-max))
             (concat (buffer-string) "\n")))))
    (if (called-interactively-p 'any)
        (insert fortune)
      fortune)))

;; initial-scratch-message
(let ((fortune (my-fortune-scratch-message)))
  (when fortune
    (setq initial-scratch-message fortune)))

;; set scratch major mode to `emacs-lisp-mode'
(setq initial-major-mode 'emacs-lisp-mode)



;;; all frame related settings
;; cursor settings
(setq-default cursor-type '(bar . 1))
;; disable annoying cursor blinks
(blink-cursor-mode -1)
;; disable the ugly toolbar
(tool-bar-mode -1)
;; silence all bell rings
(setq ring-bell-function 'ignore)
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

(setq frame-resize-pixelwise t) ; Allow frame size to inc/dec by a pixel
(setq tooltip-mode nil) ; Disable tooltip appearance on mouse hover

(setq frame-title-format
      `("emacs "
        ;; If `emacs-git-branch' is non-nil, show that
        (emacs-git-branch ,(concat "[" emacs-git-branch "]")
                          ;; Else show the version number
                          ,(concat (number-to-string emacs-major-version)
                                   "."
                                   (number-to-string emacs-minor-version)))
        "   "
        (buffer-file-name "%f" ;Show full file path if buffer is showing a file
                          (dired-directory dired-directory ;Else if in dired mode, show the directory name
                                           "%b")) ;Else show the buffer name (*scratch*, *Messages*, etc)
        " %*"))

;; set continuation indicators to right fringe only
(setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      '(nil right-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; make sure emacsclient starts at fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; make the titlebar transparent in mac
(when (is-mac-p)
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; remove ugly scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; resize windows proportionally
(setq window-combination-resize t)



;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines
;; enter page-break character in Emacs by entering `C-q C-l'
(use-package page-break-lines
  :hook ((prog-mode org-mode) . page-break-lines-mode))

;; column-enforce-mode: highlight characters which exceed fill-column
;; https://github.com/jordonbiondo/column-enforce-mode
(use-package column-enforce-mode
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'web-mode)
                                (column-enforce-mode))))
  ;; enforce a column of 80 for highlighting
  (setq column-enforce-column 80)
  (setq column-enforce-comments nil))

;; indicate buffer boundaries in the fringe
(setq-default indicate-buffer-boundaries '((top . right)
                                           (bottom . right)))

;; Easily adjust the font size in all Emacs frames
;; https://github.com/purcell/default-text-scale/
(use-package default-text-scale
  :config (default-text-scale-mode))



;; fontify-face: Fontify symbols representing faces with that face.
;; https://github.com/Fuco1/fontify-face/tree/master
(use-package fontify-face
  :defer t)

(provide 'setup-visual)
