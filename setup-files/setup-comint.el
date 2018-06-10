;; Time-stamp: <2018-06-11 02:30:01 csraghunandan>

;; comint: all inferior processes inherit from comint-mode
(use-package comint :ensure nil
  :bind
  (:map comint-mode-map
        ("C-l" . my-recenter-top-bottom)
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input ))
  :custom
  (comint-scroll-to-bottom-on-input t)
  (comint-process-echoes t "prevent comint process from echoing the command typed to the user")
  :config
  ;; makes sense to not recenter to the middle for comint buffers. Only top/bottom
  (defun my-recenter-top-bottom ()
    (interactive)
    (goto-char (point-max))
    (let ((recenter-positions '(top bottom)))
      (recenter-top-bottom))))

(provide 'setup-comint)
