;;; setup-term.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-04-18 12:31:10 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; vterm: Emacs libvterm integration
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :config

  ;; disable some unnecessary minor-modes in term-mode
  (add-hook 'vterm-mode-hook (lambda ()
                               (yas-minor-mode -1)
                               (whole-line-or-region-local-mode -1)
                               (setq-local global-hl-line-mode nil)

                               ;; Don't prompt about processes when killing vterm
                               (setq-local confirm-kill-processes nil)
                               ;; Prevent premature horizontal scrolling
                               (setq-local hscroll-margin 0)))

  ;; vterm buffers are killed when the associated process is terminated
  (setq vterm-kill-buffer-on-exit t))

;; vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.
;; https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle)

;; multi-vterm: manage multiple terminal windows easily within emacs
;; https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :load-path "~/.emacs.d/elisp/multi-vterm"
  :config
  (bind-key "C-c t"
            (defhydra multi-term-hydra ()
              "multi-term"
              ("o" multi-vterm "new terminal")
              ("t" vterm-toggle-cd "toggle/open")
              ("n" multi-vterm-next "Next")
              ("p" multi-vterm-prev "Prev")
              ("d" multi-vterm-dedicated-toggle "Dedicated terminal")
              ("r" multi-vterm-projectile "vterm projectile")
              ("q" nil "Quit" :color blue))))

(provide 'setup-term)
