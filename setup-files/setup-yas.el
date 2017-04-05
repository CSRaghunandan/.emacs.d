;; Time-stamp: <2017-04-02 22:45:44 csraghunandan>

;; yasnippet: snippets tool for emacs
;; https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (setq yas-triggers-in-field t); Enable nested triggering of snippets
  (setq yas-prompt-functions '(yas-completing-prompt))
  (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))

  (bind-key "C-c y"
            (defhydra hydra-yas (:color blue
                                        :hint nil)
"
_i_nsert    _n_ew       _v_isit     aya _c_reate
_r_eload    e_x_pand    _?_ list    aya _e_xpand
"
      ("i" yas-insert-snippet)
      ("n" yas-new-snippet)
      ("v" yas-visit-snippet-file)
      ("r" yas-reload-all)
      ("x" yas-expand)
      ("c" aya-create)
      ("e" aya-expand)
      ("?" yas-describe-tables)
      ("q" nil "cancel" :color blue)))

  ;; https://github.com/joaotavora/yasnippet/issues/794#issuecomment-290216043
  ;; `auto-fill-mode' seems to break yasnippet. Wait till the issue is fixed.
  ;; Below is a temporary workaround
  (advice-add 'yas--auto-fill-wrapper :override #'ignore))

;; auto-yasnippet: create disposable snippets on the fly
;; https://github.com/abo-abo/auto-yasnippet
(use-package auto-yasnippet)

(provide 'setup-yas)
