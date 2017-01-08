;; Time-stamp: <2017-01-08 19:31:34 csraghunandan>

;; god-mode:  minor mode for entering Emacs commands without modifier keys
;; https://github.com/chrisdone/god-mode
(use-package god-mode
  :bind* ("<escape>" . god-mode-all)
  :config
  ;; enable which-key support for god-mode
  (which-key-enable-god-mode-support)

  ;; activate god-mode in isearch
  (require 'god-mode-isearch)
  (bind-key "<escape>" 'god-mode-isearch-activate isearch-mode-map)
  (bind-key "<escape>" 'god-mode-isearch-disable god-mode-isearch-map)

  (bind-key "." 'repeat god-local-mode-map)
  (bind-key "i" 'god-local-mode god-local-mode-map))

(provide 'setup-god)
