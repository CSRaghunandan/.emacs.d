;; Time-stamp: <2017-01-03 15:02:52 csraghunandan>

;; flyspell, flycheck

;; flypsell
;; on the fly spell checking
(use-package flyspell
  :diminish flyspell-mode
  :config
  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t)

  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; use aspell as the default dictionary
  (setq ispell-program-name "aspell"))

;; flycheck: on the fly syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :diminish flycheck-mode
  :config
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Flycheck"
    ("s"  flycheck-error-list-set-filter "Filter")
    ("n"  flycheck-next-error "Next")
    ("p"  flycheck-previous-error "Previous")
    ("f" flycheck-first-error "First")
    ("l"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil "quit" :color blue))
  (bind-key "C-c h f" 'hydra-flycheck/body))

(provide 'setup-fly)
