;; Time-stamp: <2016-12-30 18:34:11 csraghunandan>

;; company, company-quickhelp, company-statistics

;; company
;; http://company-mode.github.io/
;; auto-completion backend for emacs
(use-package company
  :diminish company-mode
  :config
  (bind-keys
   :map company-active-map
   ("M-p" . nil)
   ("M-n" . nil)
   ("C-m" . nil)
   ("C-h" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common)
   ("C-t" . company-show-doc-buffer))

  ;; replace `dabbrev-expand' with `hippie-expand' which does a lot more
  (bind-key* "M-/" 'hippie-expand)

  ;; don't downcase results from company-dabbrev
  (setq company-dabbrev-downcase nil)
  ;; use only buffers with same major-mode for company-dabbrev
  (setq company-dabbrev-other-buffers t)
  (bind-key "C-<tab>" 'company-dabbrev)

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                   `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9)))

  ;; set defaults for company-mode
  (setq company-tooltip-flip-when-above t
        company-minimum-prefix-length 3
        company-idle-delay 0.4
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-tooltip-align-annotations t)

  ;; company-statistics
  ;; https://github.com/company-mode/company-statistics
  ;; sort the company candidates by the statistics
  (use-package company-statistics
    :defer 1
    :config (company-statistics-mode)))

(provide 'setup-company)
