;;; setup-term.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-30 16:27:05 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; multi-term: manage multiple terminal windows easily within emacs
;; https://github.com/emacsorphanage/multi-term/tree/f954e4e18b0a035151d34852387e724d87a3316f
(use-package multi-term
  :config
  ;; Some userlike the cursor return to the position it was before I opened the
  ;; dedicated terminal window.
  (setq multi-term-dedicated-close-back-to-open-buffer-p t)

  (defun last-term-buffer (l)
    "Return most recently used term buffer."
    (when l
      (if (eq 'term-mode (with-current-buffer (car l) major-mode))
          (car l) (last-term-buffer (cdr l)))))

  (defun get-term ()
    "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
    (interactive)
    (let ((b (last-term-buffer (buffer-list))))
      (if (or (not b) (eq 'term-mode major-mode))
          (multi-term)
        (switch-to-buffer b))))

  (bind-key "C-c t"
            (defhydra multi-term-hydra ()
              "multi-term"
              ("o" get-term "toggle/open")
              ("n" multi-term-next "Next")
              ("p" multi-term-prev "Prev")
              ("d" multi-term-dedicated-toggle "Dedicated terminal")
              ("q" nil "Quit" :color blue))))

;; eterm-256color: Customizable 256 colors for emacs term and ansi-term
;; https://github.com/dieggsy/eterm-256color
(use-package eterm-256color :defer t
  :hook ((term-mode . eterm-256color-mode)))

(use-package term :ensure nil
  :config
  ;; bind-keys for term-mode
  (setq term-bind-key-alist
        '(("C-c C-c" . term-interrupt-subjob)
          ("C-v" . scroll-up)
          ("M-v" . scroll-down)
          ("C-c C-e" . term-send-esc)
          ("C-c C-j" . term-line-mode)
          ("C-c C-k" . term-char-mode)
          ("C-b"     . term-send-left)
          ("C-f"     . term-send-right)
          ("C-p"     . previous-line)
          ("C-n"     . next-line)
          ("C-s"     . swiper)
          ("C-m"     . term-send-return)
          ("C-y"     . term-paste)
          ("M-f"     . term-send-forward-word)
          ("M-b"     . term-send-backward-word)
          ("C-h"     . term-send-backspace)
          ("M-p"     . term-send-up)
          ("M-n"     . term-send-down)
          ("M-d"     . term-send-forward-kill-word)
          ("C-M-h"   . term-send-backward-kill-word)
          ("M-r"     . term-send-reverse-search-history)
          ("M-,"     . term-send-raw)
          ("M-." . comint-dynamic-complete)))

  ;; disable some unnecessary minor-modes in term-mode
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1)
                              (whole-line-or-region-local-mode -1)
                              (setq-local global-hl-line-mode nil)
                              (hungry-delete-mode -1)))

  (setq multi-term-buffer-name "term")
  (setq multi-term-program (getenv "SHELL")
        multi-term-switch-after-close 'PREVIOUS))

(provide 'setup-term)

;; shell
;; executing `shell' with a prefix will create a new *shell* buffer
;; C-c M-o will clear comint buffers
;; `[up]' and `[down]' will cycle the previous and next inputs
