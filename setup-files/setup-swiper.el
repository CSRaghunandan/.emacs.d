;; Time-stamp: <2016-10-10 13:34:01 csraghunandan>

;; swiper
;; https://github.com/abo-abo/swiper
;; isearch with an overview!
(use-package swiper
  :bind (:map isearch-mode-map ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind
  (("M-i" . counsel-grep-or-swiper)
   ("M-I" . swiper-all))
  :config (setq swiper-action-recenter t))

(provide 'setup-swiper)
