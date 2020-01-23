;;; setup-theme.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-23 11:57:17 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; function to disable all enabled themes
(defun gh/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;;; Theme hooks
(defvar gh/theme-hooks nil
  "((theme-id . function) ...)")

(defun gh/add-theme-hook (theme-id hook-func)
  (add-to-list 'gh/theme-hooks (cons theme-id hook-func)))

(defun gh/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `gh/add-theme-hook'."
  (unless no-enable
    (gh/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id gh/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'gh/load-theme-advice)

;; ;; zenburn: A pleasing dark theme for emacs
;; ;; https://github.com/bbatsov/zenburn-emacs
;; (use-package zenburn-theme
;;   :init
;;   (defun gh/zenburn-theme-hook()
;;     ;; Moody mode-line configuration
;;     (let ((line (face-attribute 'mode-line :underline)))
;;       (zenburn-with-color-variables
;;         (set-face-attribute 'mode-line          nil :overline   line)
;;         (set-face-attribute 'mode-line-inactive nil :overline   line)
;;         (set-face-attribute 'mode-line-inactive nil :underline  line)
;;         (set-face-attribute 'mode-line          nil :box        nil)
;;         (set-face-attribute 'mode-line-inactive nil :box        nil)
;;         (set-face-attribute 'mode-line          nil :background "gray30" :weight 'bold)
;;         (set-face-attribute 'mode-line-inactive nil :background zenburn-bg-1)
;;         (set-face-attribute 'mode-line          nil :foreground zenburn-green+4)
;;         (set-face-attribute 'mode-line-inactive nil :foreground "gray70")))

;;     (with-eval-after-load "rainbow-delimiters"
;;      (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "red" :strike-through t))
;;     (with-eval-after-load "column-enforce-mode"
;;       (set-face-attribute 'column-enforce-face nil
;;                           :underline nil :foreground "firebrick3"))

;;     ;; markdown specific settings
;;     (set-face-attribute markdown-header-face-1 nil :foreground "#DFAF8F" :weight 'bold)
;;     (set-face-attribute markdown-header-face-2 nil :foreground "#BFEBBF" :weight 'bold)
;;     (set-face-attribute markdown-header-face-3 nil :foreground "#7CB8BB" :weight 'bold)
;;     (set-face-attribute markdown-header-face-4 nil :foreground "#D0BF8F" :weight 'bold)
;;     (set-face-attribute markdown-header-face-5 nil :foreground "#93E0E3" :weight 'bold)
;;     (set-face-attribute markdown-header-face-6 nil :foreground "#9FC59F" :weight 'bold)

;;     ;; make the zenburn background face darker
;;     (set-face-attribute 'default nil :foreground "#DCDCCC" :background "#383838")

;;     ;; ivy minibuffer configuration
;;     (with-eval-after-load "ivy"
;;       (set-face-attribute 'ivy-minibuffer-match-face-4 nil
;;                           :background "pink4" :underline nil)
;;       (set-face-attribute 'ivy-minibuffer-match-face-3 nil
;;                           :background "SteelBlue3" :underline nil)
;;       (set-face-attribute 'ivy-minibuffer-match-face-2 nil
;;                           :background "DarkSeaGreen4" :underline nil)
;;       (set-face-attribute 'ivy-current-match nil
;;                           :foreground "#FFECBA" :underline nil :weight 'bold))

;;     ;; magit faces
;;     (with-eval-after-load "magit"
;;       (set-face-attribute 'magit-section-highlight nil :background "gray27")
;;       (set-face-attribute 'magit-diff-file-heading-highlight nil :background "gray27")
;;       (set-face-attribute 'magit-diff-hunk-heading-highlight nil :background "gray27")
;;       (set-face-attribute 'magit-diff-context-highlight nil :background "gray27")
;;       (set-face-attribute 'magit-branch-current nil :box 'unspecified :weight 'bold)
;;       (set-face-attribute 'magit-branch-remote-head nil :box 'unspecified))

;;     ;; make function face brighter so it's easily distnguishable
;;     (set-face-attribute 'font-lock-function-name-face nil :foreground "CadetBlue1")

;;     ;; fontify links to make them standout
;;     (set-face-attribute 'link nil :foreground "#C9B8A2"
;;                         :underline nil :weight 'normal)
;;     (set-face-attribute 'link-visited nil :foreground "#C9AE8C"
;;                         :underline nil :weight 'normal)

;;     ;; make everything look gray
;;     (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "gray55")
;;     (set-face-attribute 'font-lock-comment-face nil :foreground "gray55")
;;     (set-face-attribute 'font-lock-doc-face nil :foreground "gray70")

;;     (with-eval-after-load "golden-ratio-scroll-screen"
;;       (set-face-attribute 'golden-ratio-scroll-highlight-line-face nil
;;                           :background 'unspecified :foreground 'unspecified))

;;     (with-eval-after-load "shm"
;;       (set-face-background 'shm-current-face "gray27"))

;;     (set-face-attribute 'hl-line nil :background "gray27")
;;     (set-face-attribute 'fringe nil :background "gray30")
;;     (set-face-attribute 'vhl/default-face nil :background  "gray27")
;;     (set-face-attribute 'vertical-border nil :foreground "gray20")

;;     ;; hydra configuration
;;     (set-face-attribute 'hydra-face-red nil
;;                         :foreground "#FF6956" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-blue nil
;;                         :foreground "Cyan" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-amaranth nil
;;                         :foreground "#e52b50" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-pink nil
;;                         :foreground "HotPink1" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-teal nil
;;                         :foreground "SkyBlue1" :bold t :background "#383838")

;;     ;; disable boxes for `header-line' face
;;     (set-face-attribute 'header-line nil :box nil)

;;     ;; org-mode face
;;     (with-eval-after-load "org"
;;       ;; set the face for `org-checkbox'
;;       (set-face-attribute 'org-checkbox nil :foreground "gray70" :background 'unspecified
;;                           :weight 'bold :box nil)

;;       ;; do not underline `org-date' face
;;       (set-face-underline 'org-date nil)

;;       ;; this looks better in my opinion
;;       (set-face-attribute 'org-ellipsis nil :underline 'unspecified :foreground "#E0CF9F")))

;;   (gh/add-theme-hook 'zenburn #'gh/zenburn-theme-hook)

;;   :config

;;   (defun my/load-theme (frame)
;;     (select-frame frame)
;;     (load-theme 'zenburn))
;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions #'my/load-theme)
;;     (load-theme 'zenburn)))

;; a pack of modern color themes for emacs
;; https://github.com/hlissner/emacs-doom-themes/
(use-package doom-themes
  :init
  (defun gh/doom-dark+-theme-hook()
    ;; My modifications for the doom-dark+ theme
    (set-face-attribute 'vertical-border nil :foreground "gray20")

    (with-eval-after-load "golden-ratio-scroll-screen"
      (set-face-attribute 'golden-ratio-scroll-highlight-line-face nil
                          :background 'unspecified :foreground 'unspecified))

    ;; make comments gray instead of green
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "gray40")
    (set-face-attribute 'font-lock-comment-face nil :foreground "gray40")
    (set-face-attribute 'font-lock-doc-face nil :foreground "gray55")

    ;; TODO: fix diff-hl highlighting
    )

  (gh/add-theme-hook 'doom-dark+ #'gh/doom-dark+-theme-hook)

  :config
  (defun my/load-theme (frame)
    (select-frame frame)
    (load-theme 'doom-dark+))
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/load-theme)
    (load-theme 'doom-dark+)))

(provide 'setup-theme)
