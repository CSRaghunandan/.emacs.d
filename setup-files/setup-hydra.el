;; Time-stamp: <2017-12-29 12:49:51 csraghunandan>

;; hydra: tie related commands into a family of short bindings with a common
;; prefix - a Hydra
;; https://github.com/abo-abo/hydra
(use-package hydra
  :config
  (set-face-attribute 'hydra-face-red nil
                      :foreground "#FF6956" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-blue nil
                      :foreground "Cyan" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-amaranth nil
                      :foreground "#e52b50" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-pink nil
                      :foreground "HotPink1" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-teal nil
                      :foreground "SkyBlue1" :bold t :background "#383838")
  (hydra-add-font-lock))

(provide 'setup-hydra)

;; hydras: ibuffer, origami, move, yasnippet, info, macros
;;         dired sort, bm, projectile, toggle case, langtools, kruecolor
;;         smartparens, multi-term, aprops, quickrun, org-clock, command-log
;;         magit, info-to, js2-refactor, mark-org-table, smerge-mode, font-resize
