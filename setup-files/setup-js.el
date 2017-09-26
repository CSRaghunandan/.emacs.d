;; Time-stamp: <2017-09-26 11:24:36 csraghunandan>

;; JavaScript configuration

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode)
   ("\\.jsx$" . js2-jsx-mode))
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2)

  ;; extra features for imenu
  (add-hook 'js2-mode-hook (lambda ()
                             (js2-imenu-extras-mode)))

  ;; tern: IDE like features for javascript and completion
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package tern
    :diminish tern-mode "𝐓𝐞"
    :if (executable-find "tern")
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files company-yasnippet))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (add-hook 'js2-mode-hook 'company-mode))

  (add-hook 'js2-mode-hook 'tern-mode)

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)

  ;; enable flycheck in js2-mode
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (when (executable-find "eslind_d")
    (setq flycheck-javascript-eslint-executable "eslint_d"))

  ;; company-tern: company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern
    :if (executable-find "tern")
    :config
    ;; Disable completion keybindings, as we use xref-js2 instead
    (define-key tern-mode-keymap (kbd "M-.") nil)
    (define-key tern-mode-keymap (kbd "M-,") nil))

  ;; js2-refactor: refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor
    :diminish js2-refactor-mode "𝐉𝐫"
    :bind
    (:map js2-mode-map
          ("C-k" . js2r-kill)
          ("C-c h r" . js2-refactor-hydra/body))
    :config (js2r-add-keybindings-with-prefix "C-c C-r")

    (defhydra js2-refactor-hydra (:color blue :hint nil)
      "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
      ("ee" js2r-expand-node-at-point)
      ("cc" js2r-contract-node-at-point)
      ("ef" js2r-extract-function)
      ("em" js2r-extract-method)
      ("tf" js2r-toggle-function-expression-and-declaration)
      ("ta" js2r-toggle-arrow-function-and-expression)
      ("ip" js2r-introduce-parameter)
      ("lp" js2r-localize-parameter)
      ("wi" js2r-wrap-buffer-in-iife)
      ("ig" js2r-inject-global-in-iife)
      ("ag" js2r-add-to-globals-annotation)
      ("ev" js2r-extract-var)
      ("iv" js2r-inline-var)
      ("rv" js2r-rename-var)
      ("vt" js2r-var-to-this)
      ("ao" js2r-arguments-to-object)
      ("ti" js2r-ternary-to-if)
      ("sv" js2r-split-var-declaration)
      ("ss" js2r-split-string)
      ("uw" js2r-unwrap)
      ("lt" js2r-log-this)
      ("dt" js2r-debug-this)
      ("sl" js2r-forward-slurp)
      ("ba" js2r-forward-barf)
      ("k" js2r-kill)
      ("q" nil)))

  (add-hook 'js2-mode-hook 'js2-refactor-mode)

  ;; prettier-emacs: minor-mode to prettify javascript files on save
  ;; https://github.com/prettier/prettier-emacs
  (use-package prettier-js
    :if (executable-find "prettier")
    :config
    (add-hook 'js2-mode-hook 'prettier-js-mode))

  ;; xref-js2: Jump to references/definitions using ag & js2-mode's AST in Emacs
  ;; https://github.com/nicolaspetton/xref-js2
  (use-package xref-js2
    :config

    ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
    ;; unbind it.
    (define-key js-mode-map (kbd "M-.") nil)

    (add-hook 'js2-mode-hook (lambda ()
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  ;; json-snatcher: get the path of any JSON element easily
  ;; https://github.com/Sterlingg/json-snatcher
  (use-package json-snatcher
    :config
    (defun js-mode-bindings ()
      "Sets a hotkey for using the json-snatcher plugin"
      (when (string-match  "\\.json$" (buffer-name))
        (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
    (add-hook 'js2-mode-hook 'js-mode-bindings))

  ;; indium: javascript awesome development environment
  ;; https://github.com/NicolasPetton/indium
  (use-package indium
    :config (add-hook 'js2-mode-hook 'indium-interaction-mode))

  ;; mocha: emacs mode for running mocha tests
  ;; https://github.com/scottaj/mocha.el
  (use-package mocha
    :config
    (require 'typescript-mode)
    (dolist (m (list js2-mode-map typescript-mode-map))
      (bind-keys
       :map m
       ("C-c m P" . mocha-test-project)
       ("C-c m f" . mocha-test-file)
       ("C-c m p" . mocha-test-at-point))))

  ;; mocha-snippets: snippets for mocha test framework
  ;; https://github.com/cowboyd/mocha-snippets.el
  (use-package mocha-snippets))

;; json-mode: Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :config
  (add-hook 'json-mode 'prettier-js-mode))

(provide 'setup-js)
