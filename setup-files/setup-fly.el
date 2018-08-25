;;; setup-fly.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-26 00:24:48 csraghunandan>

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
  :config
  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t)

  ;; speed up flyspell
  (setq flyspell-issue-message-flag nil)

  ;; use hunspell as the default dictionary
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell"))
    (setq ispell-really-hunspell t)
    (setenv "DICTIONARY" "en_GB")
    (setq ispell-dictionary "en_GB")))

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

;; flycheck-posframe: Show flycheck errors via posframe.el
;; https://github.com/alexmurray/flycheck-posframe
(use-package flycheck-posframe :defer t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

(provide 'setup-fly)

;; to install and configure hunspell on MacOS:
;; brew install hunspell or sudo apt install hunspell
;; cd ~/Library/Spelling/
;; wget http://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.aff
;; wget http://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_GB.dic
;; TODO: write instructions for linux and windows
