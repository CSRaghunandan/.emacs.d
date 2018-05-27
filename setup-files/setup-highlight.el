;; Time-stamp: <2018-05-27 12:23:35 csraghunandan>

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
  (beacon-mode)
  (setq beacon-size 25)
  ;; don't blink in shell-mode
  (add-to-list 'beacon-dont-blink-major-modes #'comint-mode)
  (add-to-list 'beacon-dont-blink-major-modes #'term-mode))

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
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0.05))

;; hl-todo: Highlight TODO keywords
;; https://github.com/tarsius/hl-todo/tree/master
(use-package hl-todo
  :config (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success))))

;; enable some extra syntax highlighting for dash
(with-eval-after-load 'dash
  (dash-enable-font-lock))

(provide 'setup-highlight)
