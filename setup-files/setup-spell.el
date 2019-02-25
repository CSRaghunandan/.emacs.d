;;; setup-spell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-02-26 00:21:34 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; flypsell: on the fly spell checking
(use-package flyspell
  :ensure nil
  :hook
  ((org-mode . flyspell-mode)
   (markdown-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :init
  (setq ispell-dictionary-alist
        '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t
               ("-d" "en_US" "-i" "utf-8") nil utf-8)
          ("american"
           "[A-Za-z]" "[^A-Za-z]" "[']" nil
           ("-d" "en_US") nil utf-8)
          ("british"
           "[A-Za-z]" "[^A-Za-z]" "[']" nil
           ("-d" "en_GB") nil utf-8)))
  :config
  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t)

  ;; speed up flyspell
  (setq flyspell-issue-message-flag nil)

  ;; use hunspell as the default dictionary
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell"))
    (setq ispell-dictionary "british")
    (setq ispell-really-hunspell t)))

(provide 'setup-spell)

;; to install and configure hunspell on MacOS:
;; brew install hunspell or sudo apt install hunspell
;; cd ~/Library/Spelling/
;; wget http://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.aff
;; wget http://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.dic
;; TODO: write instructions for linux and windows
;;
;; for ArchLinux, do the following to install hunspell along with is dictionaries
;; sudo pacman -S hunspell
;; sudo pacman -S hunspell-en_US hunspell-en_GB
