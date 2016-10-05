;; needed for sorting the results from fuzzy search in ivy
(use-package flx)

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind
  (:map ivy-mode-map ("C-'" . ivy-avy))
  :config
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
				(t . ivy--regex-fuzzy))))

(provide 'setup-ivy)
