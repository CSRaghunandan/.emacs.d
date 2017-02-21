;; Time-stamp: <2017-02-16 22:11:16 csraghunandan>

;; All the highlight stuff config

;; highlight-symbol: move to next/prev occurrences of symbol + highlight
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :config (highlight-symbol-nav-mode))

;; volatile-highlights: highlight specific operations like undo, yank
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; enable hl-line-mode globally
(global-hl-line-mode)

;; rainbow-mode: colorize color names in buffers
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
(use-package rainbow-mode
  :diminish (rainbow-mode . ""))

;; beacon: blink the cursor whenever scrolling or switching between windows
;; https://github.com/Malabarba/beacon
(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode)
  (setq beacon-size 25)
  ;; don't blink in shell-mode
  (add-to-list 'beacon-dont-blink-major-modes 'comint-mode))

;; column-enforce-mode: highlight characters which exceed fill-column
;; https://github.com/jordonbiondo/column-enforce-mode
(use-package column-enforce-mode
  :diminish column-enforce-mode
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'web-mode)
                                (column-enforce-mode))))
  ;; enforce a column of 80 for highlighting
  (setq column-enforce-column 80)
  (set-face-attribute 'column-enforce-face nil
                      :underline nil :foreground "firebrick3"))

;; highlight-numbers: fontify numbers
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'prelude-font-lock-comment-annotations)

(provide 'setup-highlight)
