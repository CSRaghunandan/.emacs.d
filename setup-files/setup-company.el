;; Time-stamp: <2017-04-13 14:41:04 csraghunandan>

;; company, company-quickhelp, company-statistics

;; company: auto-completion backend for emacs
;; http://company-mode.github.io/
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
  (bind-key "M-/" 'hippie-expand)

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

  ;; company-statistics: sort the company candidates by the statistics
  ;; https://github.com/company-mode/company-statistics
  (use-package company-statistics :defer 1
    :config (company-statistics-mode)))

(provide 'setup-company)

;;; company-mode
;; `C-TAB' to complete using company-dabbrev backend
;; `C-t' to view the documentation of the current completion candidate
;; `C-w' to jump to the source code of the completion candidate (does not work
;; with all major-modes)
;; `C-g' to view the documentation of the current completion candidate in minibuffer
;; `M-/' to execute `hippie-expand'
;; Press `0-9' to select that company candidate
;; Press any non matching character to quit company
