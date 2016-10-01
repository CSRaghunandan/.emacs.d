(use-package swiper
  :bind (:map isearch-mode-map
              ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind* (("M-i" . swiper)
	  ("M-I" . swiper-all)))

(provide 'setup-swiper)
