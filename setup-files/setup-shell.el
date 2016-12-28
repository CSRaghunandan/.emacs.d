;; Time-stamp: <2016-12-28 12:08:07 csraghunandan>

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
  ;; disable yasnippet mode in term
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1)
                              (setq-local global-hl-line-mode nil)
                              (beacon-mode -1)))

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
              ("q" nil "Quit" :color blue))))

(provide 'setup-shell)

;; shell
;; executing `shell' with a prefix will create a new *shell* buffer
;; C-c M-o will clear comint buffers
;; `[up]' and `[down]' will cycle the previous and next inputs
