;; Time-stamp: <2016-12-09 12:42:52 csraghunandan>

;; hydra :- This is a package for GNU Emacs that can be used to tie related
;; commands into a family of short bindings with a common prefix - a Hydra.
;; https://github.com/abo-abo/hydra

;; hydras: flycheck, ibuffer, fold, this, move, yasnippet, info, macros
;;         dired sort, bm, projectile, toggle case, langtools, kruecolor

(use-package hydra
  :config
  (set-face-attribute 'hydra-face-red      nil :foreground "Red"        :bold t)
  (set-face-attribute 'hydra-face-blue     nil :foreground "cadetblue1" :bold t)
  (set-face-attribute 'hydra-face-amaranth nil :foreground "#e52b50"    :bold t)
  (set-face-attribute 'hydra-face-pink     nil :foreground "HotPink1"   :bold t)
  (set-face-attribute 'hydra-face-teal     nil :foreground "#367588"    :bold t)
  (hydra-add-font-lock))

(provide 'setup-hydra)
