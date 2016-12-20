;; Time-stamp: <2016-12-20 11:26:08 csraghunandan>

;; ehsell config
(use-package eshell
  :config
  ;; get ivy/helm completions to work in `eshell'
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                'completion-at-point)
              (define-key eshell-mode-map (kbd "C-c M-o") #'eshell-clear-buffer)))
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
  ;; remap up and down to previous and next commands in history
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input)

  (bind-key "<tab>" 'completion-at-point shell-mode-map))

;; manage multiple terminal windows easily within emacs
;; https://www.emacswiki.org/emacs/multi-term.el
(use-package multi-term
  :config
  ;; disable yasnippet mode in term
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1)))
  (bind-key "C-c h t"
            (defhydra multi-term-hydra ()
              "multi-term"
              ("o" multi-term "Open new")
              ("n" multi-term-next "Next")
              ("p" multi-term-prev "Prev")
              ("d" multi-term-dedicated-toggle "Dedicated terminal")
              ("q" nil "Quit" :color blue))))

(provide 'setup-shell)

;; shell
;; executing `shell' with a prefix will create a new *shell* buffer
;; C-c M-o will clear comint buffers
;; `[up]' and `[down]' will cycle the previous and next inputs
