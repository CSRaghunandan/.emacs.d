;; Time-stamp: <2017-12-04 13:24:12 csraghunandan>

;; swiper: isearch with an overview!
;; https://github.com/abo-abo/swiper
(use-package swiper
  :bind (:map isearch-mode-map
              ("C-c s" . swiper-from-isearch)) ; isearch > swiper
  :config
  (setq swiper-action-recenter t)
  (bind-key "C-s" #'counsel-grep-or-swiper))

(provide 'setup-swiper)

;; swiper
;; press `M-n' to select the symbol at point in swiper
;; press `C-c s' to search regexp from isearch to swiper
;; press `C-S-s' to search all the open files(can be slow)
