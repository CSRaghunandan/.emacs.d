;; Time-stamp: <2016-10-27 17:20:09 csraghunandan>

;; company, company-quickhelp, company-statistics

;; company
;; http://company-mode.github.io/
;; auto-completion backend for emacs
(use-package company
  :diminish company-mode
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common))

  ;; set defaults for company-mode
  (setq company-tooltip-flip-when-above t
	company-minimum-prefix-length 2
	company-idle-delay 0.2
	company-selection-wrap-around t
	company-show-numbers t
	company-auto-complete nil
        company-tooltip-align-annotations t)

  ;; company-quickhelp
  ;; https://github.com/expez/company-quickhelp
  ;; pop-up documentation for completion candidates
  (use-package company-quickhelp
    :config (company-quickhelp-mode 1))

  ;; company-statistics
  ;; https://github.com/company-mode/company-statistics
  ;; sort the company candidates by the statistics
  (use-package company-statistics
    :defer 1
    :config (company-statistics-mode)))

(provide 'setup-company)
