;; Time-stamp: <2017-12-05 00:59:49 csraghunandan>

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
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
;; `C-c s' -> to search regexp from isearch to swiper
