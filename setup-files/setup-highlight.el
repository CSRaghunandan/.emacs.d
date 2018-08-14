;;; setup-highlight.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:58:00 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; All the highlight stuff config

;; highlight-symbol: move to next/prev occurrences of symbol + highlight
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :config (highlight-symbol-nav-mode))

;; volatile-highlights: highlight specific operations like undo, yank
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; enable hl-line-mode globally
(global-hl-line-mode)

;; rainbow-mode: colorize color names in buffers
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
(use-package rainbow-mode
  :hook ((helpful-mode . rainbow-mode)))

;; beacon: blink the cursor whenever scrolling or switching between windows
;; https://github.com/Malabarba/beacon
(use-package beacon
  :config
  (setq beacon-size 15)
  (beacon-mode)

  ;; don't blink in shell-mode
  (add-to-list 'beacon-dont-blink-major-modes #'comint-mode t)
  (add-to-list 'beacon-dont-blink-major-modes #'term-mode t)
  (add-to-list 'beacon-dont-blink-major-modes #'sql-interactive-mode t))

;; highlight-numbers: fontify numbers
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)))

;; highlight-indent-guides: best indent guides solution for emacs
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\ǀ)
  (setq highlight-indent-guides-responsive 'top)

  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (defun jay/cleanup-hig-strings (x)
    (remove-text-properties 0 (length x) '(highlight-indent-guides-prop nil display) x))
  (advice-add 'ivy-cleanup-string :after #'jay/cleanup-hig-strings))

;; hl-todo: Highlight TODO keywords
;; https://github.com/tarsius/hl-todo/tree/master
(use-package hl-todo
  :config (global-hl-todo-mode))

;; enable some extra syntax highlighting for dash
(with-eval-after-load 'dash
  (dash-enable-font-lock))

(provide 'setup-highlight)
