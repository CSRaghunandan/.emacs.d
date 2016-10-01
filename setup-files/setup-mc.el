(define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)
;; Usually, both `C-x C-;' and `C-x RET' invoke the
;; `mule-keymap', but that's a waste of keys. Here we put it
;; _just_ under `C-x RET'.
(define-key ctl-x-map (kbd "<return>") mule-keymap)

(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
(global-set-key (kbd "M-#") #'mc/unmark-next-like-this)
(global-set-key (kbd "M-$") #'mc/unmark-previous-like-this)

(define-prefix-command 'endless/mc-map)
;; C-x m is usually `compose-mail'. Bind it to something
;; else if you use this command.
(define-key ctl-x-map "m" 'endless/mc-map)

;;; Really really nice!
(define-key endless/mc-map "i" #'mc/insert-numbers)
(define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
(define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
(define-key endless/mc-map "d"
  #'mc/mark-all-symbols-like-this-in-defun)
(define-key endless/mc-map "r" #'mc/reverse-regions)
(define-key endless/mc-map "s" #'mc/sort-regions)
(define-key endless/mc-map "l" #'mc/edit-lines)
(define-key endless/mc-map "\C-a"
  #'mc/edit-beginnings-of-lines)
(define-key endless/mc-map "\C-e"
  #'mc/edit-ends-of-lines)
(add-hook 'prog-mode-hook 'multiple-cursors-mode)

(provide 'setup-mc)
