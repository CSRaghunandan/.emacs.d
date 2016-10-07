;; Time-stamp: <2016-10-07 11:20:11 csraghunandan>

;; swiper
;; https://github.com/abo-abo/swiper
;; isearch with an overview!
(use-package swiper
  :bind (:map isearch-mode-map ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind
  (("M-i" . counsel-grep-or-swiper)
   ("M-I" . swiper-all)))

(provide 'setup-swiper)
