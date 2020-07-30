;;; setup-mode-line.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-07-30 10:21:46 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;;  Run M-x all-the-icons-install-fonts to install the fonts needed for
;;  all-the-icons package
(use-package all-the-icons)

;; https://github.com/tarsius/minions
;; A minor-mode menu for the mode line
(use-package minions
  :config (minions-mode 1))

;;  A fancy and fast mode-line inspired by minimalism design.
;; https://github.com/seagle0128/doom-modeline/
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config

  ;; enable size indication mode
  (size-indication-mode)

  ;; enable column number mode
  (column-number-mode)

  ;; use ffip for detecting files in project
  ;; This is needed as projectile as issues with symlinks for doom-modeline
  (setq doom-modeline-project-detection 'ffip)

  ;; use shorter method to display buffer filenames in doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)

  ;; don't display persp, don't have it installed
  (setq doom-modeline-persp-name nil)

  ;; show minor modes in mode-line, since minions is enabled, it will use
  ;; minions to display all the enabled minor-modes
  (setq doom-modeline-minor-modes t)

  ;; Whether display icons in mode-line. Respects `all-the-icons-color-icons'.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon t)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 3)

  ;; no need of modal-icons for doom-modeline
  (setq doom-modeline-modal-icon nil)

  ;; Don't display environment version
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-env-enable-go nil)

  ;; enable word counts for text based modes
  (setq doom-modeline-enable-word-count t))

;; macro to rename mode-name for major-modes
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "typescript-mode" typescript-mode "TS")
(rename-modeline "haskell-mode" haskell-mode "𝞴=")

(provide 'setup-mode-line)
