;; hydras:- flycheck, ibuffer, fold, google this, move, yasnippet,
;;          window management, bm

(use-package hydra
  :config
  (progn
    (set-face-attribute 'hydra-face-red      nil :foreground "Red"        :bold t)
    (set-face-attribute 'hydra-face-blue     nil :foreground "RoyalBlue3" :bold t)
    (set-face-attribute 'hydra-face-amaranth nil :foreground "#e52b50"    :bold t)
    (set-face-attribute 'hydra-face-pink     nil :foreground "HotPink1"   :bold t)
    (set-face-attribute 'hydra-face-teal     nil :foreground "#367588"    :bold t)
    (hydra-add-font-lock)))

(provide 'setup-hydra)
