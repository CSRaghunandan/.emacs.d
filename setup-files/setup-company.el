(use-package company
  :diminish company-mode
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common))

  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (setq company-tooltip-flip-when-above t
	company-minimum-prefix-length 2
	company-idle-delay 0.2
	company-selection-wrap-around t
	company-show-numbers t
	company-auto-complete nil
        company-tooltip-align-annotations t))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

(use-package company-statistics
  :defer 1
  :config (company-statistics-mode))

(provide 'setup-company)
