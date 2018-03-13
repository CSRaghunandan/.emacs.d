;; Time-stamp: <2018-03-13 23:17:09 csraghunandan>

;; flyspell, flycheck

;; flypsell: on the fly spell checking
(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
   (org-mode . flyspell-mode))
  :config
  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t)

  ;; speed up flyspell
  (setq flyspell-issue-message-flag nil)

  ;; use aspell as the default dictionary
  (setq ispell-program-name "aspell"))

;; flycheck: on the fly syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck)

;; flycheck-posframe: Show flycheck errors via posframe.el
;; https://github.com/alexmurray/flycheck-posframe
(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

(provide 'setup-fly)
