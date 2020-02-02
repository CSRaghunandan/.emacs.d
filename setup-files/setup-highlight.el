;;; setup-highlight.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-02 16:02:59 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
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
  :hook ((helpful-mode . rainbow-mode)
         (web-mode . rainbow-mode))
  :config
  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-highlight.el#L192
  ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  (with-no-warnings
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

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

  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40#issuecomment-451553492
  (defadvice ivy-cleanup-string (after my-ivy-cleanup-hig activate)
    (let ((pos 0) (next 0) (limit (length str)) (prop 'highlight-indent-guides-prop))
      (while (and pos next)
        (setq next (text-property-not-all pos limit prop nil str))
        (when next
          (setq pos (text-property-any next limit prop nil str))
          (remove-text-properties next pos '(display nil face nil) str))))))

;; hl-todo: Highlight TODO keywords
;; https://github.com/tarsius/hl-todo/tree/master
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; enable some extra syntax highlighting for dash
(with-eval-after-load 'dash
  (dash-enable-font-lock))

;; Highlight escape sequences in Emacs
;; https://github.com/dgutov/highlight-escape-sequences/
(use-package highlight-escape-sequences
  :config (hes-mode))

(provide 'setup-highlight)
