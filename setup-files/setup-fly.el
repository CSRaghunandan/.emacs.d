;; Time-stamp: <2018-05-17 19:29:56 csraghunandan>

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
(use-package flycheck)

;; flycheck-posframe: Show flycheck errors via posframe.el
;; https://github.com/alexmurray/flycheck-posframe
(use-package flycheck-posframe :defer t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

(provide 'setup-fly)
