;;; setup-company.el -*- lexical-binding: t; -*-
;; Time-stamp: <2019-01-31 11:52:03 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; company, company-quickhelp, company-statistics

;; company: auto-completion backend for emacs
;; http://company-mode.github.io/
(use-package company
  :bind
  (("M-/" . hippie-expand) ;; replace `dabbrev-expand' with `hippie-expand' which does a lot more
   ("C-<tab>" . company-dabbrev))
  (:map company-active-map
        ("M-p" . nil)
        ("M-n" . nil)
        ("C-m" . nil)
        ("C-h" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common)
        ("C-f" . company-complete-common)
        ("C-t" . company-show-doc-buffer))
  :config
  (setq company-tooltip-flip-when-above t)
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-require-match 'never)
  (setq company-tooltip-align-annotations t)

  ;; don't downcase results from company-dabbrev
  (setq company-dabbrev-downcase nil)
  ;; use only buffers with same major-mode for company-dabbrev
  (setq company-dabbrev-other-buffers t)

  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  (defvar sanityinc/page-break-lines-on-p nil)
  (make-variable-buffer-local 'sanityinc/page-break-lines-on-p)

  (defun sanityinc/page-break-lines-disable (&rest ignore)
    (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
      (page-break-lines-mode -1)))

  (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
    (when sanityinc/page-break-lines-on-p
      (page-break-lines-mode 1)))

  (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
  (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable))

;; company-prescient: Simple but effective sorting and filtering for Emacs.
;; https://github.com/raxod502/prescient.el/tree/master
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config (prescient-persist-mode +1))

;; company-quickhelp: documentation popup for company
;; https://github.com/expez/company-quickhelp/tree/master
(use-package company-quickhelp
  :after company
  :config
  (when (is-linux-p)
    (company-quickhelp-mode)))

(provide 'setup-company)

;;; company-mode
;; `C-TAB' to complete using company-dabbrev backend
;; `C-t' to view the documentation of the current completion candidate
;; `C-w' to jump to the source code of the completion candidate (does not work
;; with all major-modes)
;; `M-/' to execute `hippie-expand'
;; Press any non matching character to quit company
