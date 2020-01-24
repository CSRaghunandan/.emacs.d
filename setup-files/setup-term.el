;;; setup-term.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-22 14:48:20 csraghunandan>

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
                               (hungry-delete-mode -1)

                               ;; Don't prompt about processes when killing vterm
                               (setq-local confirm-kill-processes nil)
                               ;; Prevent premature horizontal scrolling
                               (setq-local hscroll-margin 0)
                               )))

;; vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.
;; https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle)

;; multi-libvterm: manage multiple terminal windows easily within emacs
;; https://github.com/suonlight/multi-libvterm
(use-package multi-libvterm
  :load-path "~/.emacs.d/elisp/multi-libvterm"
  :config
  (bind-key "C-c t"
            (defhydra multi-term-hydra ()
              "multi-term"
              ("o" multi-libvterm "new terminal")
              ("t" vterm-toggle-cd "toggle/open")
              ("n" multi-libvterm-next "Next")
              ("p" multi-libvterm-prev "Prev")
              ("d" multi-libvterm-dedicated-toggle "Dedicated terminal")
              ("r" multi-libvterm-projectile "vterm projectile")
              ("q" nil "Quit" :color blue))))

;; eterm-256color: Customizable 256 colors for emacs term and ansi-term
;; https://github.com/dieggsy/eterm-256color
(use-package eterm-256color :defer t
  :hook ((vterm-mode . eterm-256color-mode)))

(provide 'setup-term)

;; shell
;; executing `shell' with a prefix will create a new *shell* buffer
;; C-c M-o will clear comint buffers
;; `[up]' and `[down]' will cycle the previous and next inputs
