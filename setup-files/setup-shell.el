;; Time-stamp: <2016-12-02 15:37:52 csraghunandan>

;; ehsell config
(use-package eshell
  :config
  ;; get ivy/helm completions to work in `eshell'
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                'completion-at-point)))
  ;; fetch the $PATH variable to eshell
  (add-hook 'eshell-mode-hook '(lambda ()(exec-path-from-shell-initialize)))

  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (add-hook 'eshell-mode-hook
            '(lambda()
               (local-set-key (kbd "C-l") 'eshell-clear-buffer))))

;; always insert at bottom
(setq comint-scroll-to-bottom-on-input t)
;; remap up and down to previous and next commands
(define-key comint-mode-map [up] 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-input)

(provide 'setup-shell)

;; shell
;; executing `shell' with a prefix will create a new *shell* buffer
;; C-c M-o will clear the `shell' buffer
;; `[up]' and `[down]' will cycle the previous and next inputs
