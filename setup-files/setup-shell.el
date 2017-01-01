;; Time-stamp: <2017-01-01 13:46:08 csraghunandan>

;; ehsell config
(use-package eshell
  :config
  ;; get ivy/helm completions to work in `eshell'
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys
               :map eshell-mode-map
               ("<tab>" . completion-at-point)
               ("C-c M-o" . eshell-clear-buffer))))
  ;; fetch the $PATH variable to eshell
  (add-hook 'eshell-mode-hook '(lambda ()(exec-path-from-shell-initialize)))

  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

;; handle all inferior processes/shell settings
(use-package comint :ensure nil
  :config
  ;; always insert input at bottom
  (setq comint-scroll-to-bottom-on-input t)

  ;; makes sense to not recenter to the middle for comint buffers. Only top/bottom
  (defun my-recenter-top-bottom ()
    (interactive)
    (goto-char (point-max))
    (let ((recenter-positions '(top bottom)))
      (recenter-top-bottom)))

  (bind-key "C-l" 'my-recenter-top-bottom comint-mode-map)

  ;; prevent comint process from echoing the command typed to the user
  (setq-default comint-process-echoes t)

  ;; remap up and down to previous and next commands in history
  (bind-keys
   :map comint-mode-map
   ("<up>" . comint-previous-input)
   ("<down>" . comint-next-input)))

;; manage multiple terminal windows easily within emacs
;; https://www.emacswiki.org/emacs/multi-term.el
(use-package multi-term
  :config

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

  (bind-key "C-c h t"
            (defhydra multi-term-hydra ()
              "multi-term"
              ("o" get-term "toggle/open")
              ("n" multi-term-next "Next")
              ("p" multi-term-prev "Prev")
              ("d" multi-term-dedicated-toggle "Dedicated terminal")
              ("q" nil "Quit" :color blue)))

  (use-package term
    :config

    ;; bind-keys for term-mode
    (setq term-bind-key-alist
          '(("C-c C-c" . term-interrupt-subjob)
            ("C-c C-e" . term-send-esc)
            ("C-c C-j" . term-line-mode)
            ("C-c C-k" . term-char-mode)
            ("C-a"     . term-bol)
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
                                (setq-local global-hl-line-mode nil)
                                (beacon-mode -1)
                                (hungry-delete-mode -1)))

    (setq multi-term-buffer-name "term")))

(provide 'setup-shell)

;; shell
;; executing `shell' with a prefix will create a new *shell* buffer
;; C-c M-o will clear comint buffers
;; `[up]' and `[down]' will cycle the previous and next inputs
