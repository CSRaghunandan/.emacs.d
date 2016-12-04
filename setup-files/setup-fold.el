;; Time-stamp: <2016-12-04 00:08:20 csraghunandan>

;; fold-dwim
;; https://github.com/emacsmirror/fold-dwim
;; fold based on syntax aka, fold do what I mean
(use-package fold-dwim
  :bind ("C-c t f" . fold-dwim-toggle)
  :config
  (defhydra hydra-fold (:pre (hs-minor-mode 1))
    "fold"
    ("t" fold-dwim-toggle "toggle")
    ("h" fold-dwim-hide-all "hide-all")
    ("s" fold-dwim-show-all "show-all")
    ("q" nil "quit"))
  (bind-key "C-c h d" 'hydra-fold/body))

(provide 'setup-fold)
