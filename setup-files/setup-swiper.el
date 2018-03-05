;; Time-stamp: <2018-03-05 10:57:37 csraghunandan>

;; swiper: isearch with an overview!
;; https://github.com/abo-abo/swiper
(use-package swiper
  :bind (("C-s" . counsel-grep-or-swiper)
         :map isearch-mode-map
         ("C-c s" . swiper-from-isearch)) ; isearch > swiper
  :config
  (setq swiper-action-recenter t))

(provide 'setup-swiper)

;; swiper
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
;; `C-c s' -> to search regexp from isearch to swiper
