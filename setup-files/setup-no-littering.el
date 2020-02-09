;;; setup-no-littering.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-09 23:59:50 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; Help keeping ~/.emacs.d clean
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :config

  ;; auto-save files in its own directory
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; save custom file to a separate file
  (setq custom-file (no-littering-expand-etc-file-name "custom-settings.el"))
  (load custom-file :noerror :nomessage) ; load custom-file silently
  )

(provide 'setup-no-littering)
