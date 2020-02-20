;;; setup-highlight.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-14 16:14:34 csraghunandan>

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
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

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
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-responsive 'top)

  ;; https://github.com/seagle0128/.emacs.d/blob/36381ec7d7724a7ace6d9995ec8f59f5d9a33871/lisp/init-highlight.el#L161
  ;; Don't display first level of indentation
  (defun my-indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function
        #'my-indent-guides-for-all-but-first-column)

  ;; https://github.com/seagle0128/.emacs.d/blob/36381ec7d7724a7ace6d9995ec8f59f5d9a33871/lisp/init-highlight.el#L168
  ;; Don't display indentations in `swiper'
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (with-eval-after-load 'ivy
    (defun my-ivy-cleanup-indentation (str)
      "Clean up indentation highlighting in ivy minibuffer."
      (let ((pos 0)
            (next 0)
            (limit (length str))
            (prop 'highlight-indent-guides-prop))
        (while (and pos next)
          (setq next (text-property-not-all pos limit prop nil str))
          (when next
            (setq pos (text-property-any next limit prop nil str))
            (ignore-errors
              (remove-text-properties next pos '(display nil face nil) str))))))
    (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))

  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-highlight.el#L147
  ;; Don't display indentations while editing with `company'
  (with-eval-after-load 'company
    (add-hook 'company-completion-started-hook
              (lambda (&rest _)
                "Trun off indentation highlighting."
                (when highlight-indent-guides-mode
                  (highlight-indent-guides-mode -1))))
    (add-hook 'company-after-completion-hook
              (lambda (&rest _)
                "Trun on indentation highlighting."
                (when (and (derived-mode-p 'prog-mode)
                           (not highlight-indent-guides-mode))
                  (highlight-indent-guides-mode 1))))))

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
