;;; setup-flycheck.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-04 22:05:40 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; flycheck configuration

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
    ("q" nil "Quit" :color blue)
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

(provide 'setup-flycheck)
