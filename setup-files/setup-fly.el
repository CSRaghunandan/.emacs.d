;; Time-stamp: <2018-03-04 19:55:57 csraghunandan>

;; flyspell, flycheck

;; flypsell: on the fly spell checking
(use-package flyspell
  :diminish flyspell-mode
  :config
  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t)

  ;; speed up flyspell
  (setq flyspell-issue-message-flag nil)

  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; use aspell as the default dictionary
  (setq ispell-program-name "aspell"))

;; flycheck: on the fly syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :diminish flycheck-mode)

;; flycheck-posframe: Show flycheck errors via posframe.el
;; https://github.com/alexmurray/flycheck-posframe
(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))

(provide 'setup-fly)
