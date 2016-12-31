;; Time-stamp: <2016-12-31 14:57:49 csraghunandan>

;; yasnippet - snippets tool for emacs
;; https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (setq yas-triggers-in-field t); Enable nested triggering of snippets
  (setq yas-prompt-functions '(yas-completing-prompt))
  (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))

  (bind-key "C-c h y" (defhydra hydra-yas (:color blue
                               :hint nil)
     "
[yasnippet]        _i_nsert        _n_ew        _v_isit snippet file        _r_eload all        e_x_pand        _?_ list snippets        "
      ("i" yas-insert-snippet)
      ("n" yas-new-snippet)
      ("v" yas-visit-snippet-file)
      ("r" yas-reload-all)
      ("x" yas-expand)
      ("?" yas-describe-tables)
      ("q" nil "cancel" :color blue))))

;; auto-yasnippet: create disposable snippets on the fly
;; https://github.com/abo-abo/auto-yasnippet
(use-package auto-yasnippet
  :config
  (bind-keys
   ("C-c y c" . aya-create)
   ("C-c y e" . aya-expand)))

(provide 'setup-yas)
