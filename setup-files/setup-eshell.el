;;; setup-eshell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:56:27 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; ehsell config
(use-package eshell
  :straight nil
  :config

  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys
               :map eshell-mode-map
               ("<tab>" . completion-at-point)
               ("C-c M-o" . eshell-clear-buffer))))

  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

(provide 'setup-eshell)
