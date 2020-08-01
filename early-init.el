;;; early-init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-07-30 15:38:51 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; no need of package-quickstart since we are using straight.el for package
;; management in emacs
(setq package-quickstart nil)
;; disable package enable at startup, since I'm using `straight.el'
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; comment out this line if you are not using emacs native compilation branch
;; native compile elisp files as they are loaded
(setq comp-deferred-compilation t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
