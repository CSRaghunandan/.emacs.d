;; Time-stamp: <2018-07-30 14:05:16 csraghunandan>

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

  ;; use aspell as the default dictionary
  (setq ispell-program-name "aspell"))

;; flyspell-lazy: Improve Emacs flyspell responsiveness using idle timers
;; https://github.com/rolandwalker/flyspell-lazy/tree/master
(use-package flyspell-lazy
  :after flyspell
  :config
  (flyspell-lazy-mode 1)
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

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
