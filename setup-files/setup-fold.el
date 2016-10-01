(use-package fold-dwim
  :bind* ("C-c C-f" . fold-dwim-toggle)
  :config
  (defhydra hydra-fold (:pre (hs-minor-mode 1))
    "fold"
    ("t" fold-dwim-toggle "toggle")
    ("h" fold-dwim-hide-all "hide-all")
    ("s" fold-dwim-show-all "show-all")
    ("q" nil "quit"))
  (bind-key "C-c f t" 'hydra-fold/body))

(provide 'setup-fold)
