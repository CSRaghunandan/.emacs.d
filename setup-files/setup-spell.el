;;; setup-spell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-21 18:08:59 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; flypsell: on the fly spell checking
(use-package flyspell
  :straight nil
  :hook
  (((org-mode
     markdown-mode
     TeX-mode
     rst-mode
     git-commit-mode) . flyspell-mode)
   ((prog-mode yaml-mode conf-mode) . flyspell-prog-mode))
  :config

  (setq flyspell-issue-welcome-flag nil
        ;; Significantly speeds up flyspell, which would otherwise print
        ;; messages for every word when checking the entire buffer
        flyspell-issue-message-flag nil)

  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t)

  ;; speed up flyspell
  (setq flyspell-issue-message-flag nil)

  ;; use hunspell as the default dictionary
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell"))
    (setq ispell-dictionary "british")
    (setq ispell-really-hunspell t)))

;; flyspell-lazy: Improve Emacs flyspell responsiveness using idle timers
;; https://github.com/rolandwalker/flyspell-lazy/
(use-package flyspell-lazy
  :after flyspell
  :config
    (setq flyspell-lazy-idle-seconds 1
          flyspell-lazy-window-idle-seconds 3)
    (flyspell-lazy-mode +1))

(provide 'setup-spell)

;; to install and configure hunspell on MacOS:
;; brew install hunspell or sudo apt install hunspell
;; cd ~/Library/Spelling/
;; wget http://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.aff
;; wget http://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.dic
;;
;; for ArchLinux, do the following to install hunspell along with is dictionaries
;; sudo pacman -S hunspell
;; sudo pacman -S hunspell-en_US hunspell-en_GB
