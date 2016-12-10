;; Time-stamp: <2016-12-10 19:21:15 csraghunandan>

;; flyspell, flycheck

;; flypsell
;; on the fly spell checking
(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; correct typos using ivy
  (use-package flyspell-correct-ivy
    :bind* (("C-;" . flyspell-correct-previous-word-generic))))

;; flycheck
;; http://www.flycheck.org/en/latest/
;; on the fly syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line))

  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  (bind-key "C-c h f" 'hydra-flycheck/body))

(provide 'setup-fly)
