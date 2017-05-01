;; TIme-stamp: <2017-05-01 17:31:30>

;; set PragmataPro font only if it available
(defun rag-set-face (frame)
  "Configure faces on frame creation"
  (select-frame frame)
  (if (display-graphic-p)
      (progn
        (when (member "PragmataPro" (font-family-list))
          (set-frame-font "PragmataPro-13")))))
(add-hook 'after-make-frame-functions 'rag-set-face)

;; set frame font when running emacs normally
(when (member "PragmataPro" (font-family-list))
  (set-frame-font "PragmataPro-13"))

(provide 'setup-font-check)
