;;; setup-fly.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-01-07 18:35:46 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; flyspell, flycheck

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

;; flycheck: on the fly syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :bind ("C-c h f" . hydra-flycheck/body)
  :config
  (defhydra hydra-flycheck (:color blue
                                   :hint nil)
    "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("<" flycheck-previous-error :color pink)
    (">" flycheck-next-error :color pink)
    ("?" flycheck-describe-checker)
    ("M" flycheck-manual)
    ("d" flycheck-disable-checker)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors)
    ("m" flycheck-mode)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup)))

(provide 'setup-fly)

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
