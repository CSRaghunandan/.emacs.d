;; Time-stamp: <2017-01-05 15:02:00 csraghunandan>

;; hydra :- This is a package for GNU Emacs that can be used to tie related
;; commands into a family of short bindings with a common prefix - a Hydra.
;; https://github.com/abo-abo/hydra

;; hydras: flycheck, ibuffer, origami, move, yasnippet, info, macros
;;         dired sort, bm, projectile, toggle case, langtools, kruecolor
;;         smartparens, multi-term, aprops

(use-package hydra
  :config
  (set-face-attribute 'hydra-face-red nil :foreground "#FF6956" :bold t)
  (set-face-attribute 'hydra-face-blue nil :foreground "Cyan" :bold t)
  (set-face-attribute 'hydra-face-amaranth nil :foreground "#e52b50" :bold t)
  (set-face-attribute 'hydra-face-pink nil :foreground "HotPink1" :bold t)
  (set-face-attribute 'hydra-face-teal nil :foreground "SkyBlue1" :bold t)
  (hydra-add-font-lock))

(provide 'setup-hydra)
