;; Time-stamp: <2016-11-08 21:41:01 csraghunandan>

;; flx, ivy

;; flx - fuzzy sorting heuristics algorithm
;; needed for sorting the results from fuzzy search in ivy
(use-package flx)

;; ivy
;; incremental narrowing framework for emacs
(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind
  (:map ivy-mode-map
        ("C-:" . ivy-avy))
  :config
  ;; M-w when in ivy minibuffer will copy all the completion candidates to kill ring.
  (bind-key "C-c C" 'ivy-kill-ring-save ivy-mode-map)
  (setq ivy-use-virtual-buffers t
	ivy-height 13
	ivy-initial-inputs-alist nil
	ivy-count-format ""
	ivy-virtual-abbreviate 'full ; Show the full virtual file paths
	ivy-extra-directories '("./") ; default value: ("../" "./")
	ivy-wrap t)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(counsel-ag . ivy--regex-plus)
				(counsel-grep-or-swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy)))

  ;; use ivy to jump between pages defined in the buffer
  ;; https://github.com/igorepst/ivy-pages
  (use-package ivy-pages))

(provide 'setup-ivy)
