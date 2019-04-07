;;; init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-04-07 18:34:33 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar rag--file-name-handler-alist file-name-handler-alist)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun rag-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold gc-cons-threshold--orig
        gc-cons-percentage 0.1
        file-name-handler-alist rag--file-name-handler-alist))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'load-path (concat user-emacs-directory "my-elisp-code"))

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

;; save custom file to a separate directory
(setq custom-file (concat user-emacs-directory "my-elisp-code/custom-settings.el"))
(load custom-file :noerror :nomessage) ; load custom-file silently
(load (locate-user-emacs-file "general.el") nil :nomessage)

;; run package-initialize if running emacs version < 27
(>=e "27.0"
    nil
  (package-initialize))



;; load all use-package related configuration
(load (locate-user-emacs-file "setup-packages.el") nil :nomessage)

(require 'setup-osx)
(require 'setup-org)
(require 'setup-backup)
(require 'setup-selected)
(require 'setup-treemacs)
(require 'setup-search)
(require 'setup-rg)
(require 'setup-ibuffer)
(require 'setup-recentf)
(require 'setup-desktop)
(require 'setup-calc)
(require 'setup-ediff)
(require 'setup-dired)
(require 'setup-elisp-mode)
(require 'setup-flycheck)
(require 'setup-spell)
(require 'setup-bookmark)
(require 'setup-hydra)
(require 'setup-company)
(require 'setup-smartparens)
(require 'setup-git-stuff)
(require 'setup-avy)
(require 'setup-ace-window)
(require 'setup-projectile)
(require 'setup-yas)
(require 'setup-buffers)
(require 'setup-ivy)
(require 'setup-counsel)
(require 'setup-swiper)
(require 'setup-movement)
(require 'setup-markdown)
(require 'setup-highlight)
(require 'setup-info)
(require 'setup-mode-line)
(require 'setup-editing)
(require 'setup-racket)
(require 'setup-rust)
(require 'setup-lsp)
(require 'setup-cc)
(require 'setup-haskell)
(require 'setup-python)
(require 'setup-tex)
(require 'setup-origami)
(require 'setup-duplicate-line)
(require 'setup-white-space)
(require 'setup-mc)
(require 'setup-js)
(require 'setup-typescript)
(require 'setup-ocaml)
(require 'setup-web-mode)
(require 'setup-css)
(require 'setup-eshell)
(require 'setup-comint)
(require 'setup-term)
(require 'setup-which-key)
(require 'setup-kurecolor)
(require 'setup-erc)
(require 'setup-font-check)
(require 'setup-misc)
(require 'setup-visual)
(require 'setup-tramp)
(require 'setup-zenburn)
(require 'setup-command-log-mode)
(require 'setup-calendar)
(require 'setup-minibuffer)
(require 'setup-purescript)
(require 'setup-abbrev)
(require 'setup-compile)
(require 'setup-macro)
(require 'setup-help)
(require 'setup-tldr)
(require 'setup-config-files)
(require 'setup-shell)
(require 'setup-smerge)
(require 'setup-nov)
(require 'setup-xkcd)
(require 'setup-docker)
(require 'setup-pdf)
(require 'setup-engine-mode)

;; install all packages (if they already not installed by use-package)
(package-install-selected-packages)



;; start emacs server only it has not already been started
(require 'server)
(unless (server-running-p) (server-start))

;; set gc-cons-threshold back to original value
(add-hook 'emacs-startup-hook #'rag-set-gc-threshold)

;;; init.el ends here
