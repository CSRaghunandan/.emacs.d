;;; setup-theme.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-07 18:24:11 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
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

;; a pack of modern color themes for emacs
;; https://github.com/hlissner/emacs-doom-themes/
(use-package doom-themes
  :init
  (defun gh/doom-challenger-deep-theme-hook()

    ;; don't use obnoxious colors for `golden-ratio-scroll'
    (with-eval-after-load "golden-ratio-scroll-screen"
      (set-face-attribute 'golden-ratio-scroll-highlight-line-face nil
                          :background 'unspecified :foreground 'unspecified))

    ;; make volatile highlights have the same face as region, comments are
    ;; intangible inside volatile highlights face
    (set-face-attribute 'vhl/default-face nil :background 'unspecified :inherit 'region))
  (gh/add-theme-hook 'doom-challenger-deep #'gh/doom-challenger-deep-theme-hook)

  :config

  ;; load my theme: doom-dark+
  (defun my/load-theme (frame)
    (select-frame frame)
    (load-theme 'doom-challenger-deep))
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/load-theme)
    (load-theme 'doom-challenger-deep))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; use the colorful treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  ;; use my font instead of the default variable pitch font used by
  ;; doom-themes-treemacs-theme
  (setq doom-themes-treemacs-enable-variable-pitch nil)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'setup-theme)
