;; Time-stamp: <2017-01-05 15:48:40 csraghunandan>

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

(provide 'setup-comint)
