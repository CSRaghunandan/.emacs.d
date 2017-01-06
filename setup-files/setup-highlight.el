;; Time-stamp: <2017-01-06 13:04:20 csraghunandan>

;; All the highlight stuff config

;; highlight the symbol at point + move to next/previous occurrence of symbol
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :config
  (highlight-symbol-nav-mode))

;; highlight specific operations like undo, yank
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; enable hl-line-mode globally
(global-hl-line-mode)

;; best solution for highlighting indent guides so far in emacs
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'web-mode)
                                (highlight-indent-guides-mode))))
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?∣)
  ;; make indent guides a bit more brighter
  (setq highlight-indent-guides-auto-character-face-perc 15))

;; colorize color names in buffers
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
(use-package rainbow-mode
  :diminish (rainbow-mode . "𝐑𝐚"))

;; beacon :-  blink the cursor whenever scrolling or switching between windows
;; https://github.com/Malabarba/beacon
(use-package beacon
  :defer 1
  :diminish beacon-mode
  :bind (("C-!" . beacon-blink))
  :config
  (beacon-mode)
  (setq beacon-size 25)
  ;; don't blink in shell-mode
  (add-to-list 'beacon-dont-blink-major-modes 'comint-mode))

;; highlight characters which exceed the column limit
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

;; highlight-numbers in buffers
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
