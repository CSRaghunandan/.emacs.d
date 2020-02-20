;;; setup-js.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-24 11:56:01 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; JavaScript configuration

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :hook ((js2-mode . (lambda ()
                       (flycheck-mode)
                       (my-tide-setup-hook)
                       (company-mode))))
  :config

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-project-root-files "package.json"))

  ;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t)

  ;; use eslint_d insetad of eslint for faster linting
  (setq flycheck-javascript-eslint-executable "eslint_d")

  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)
  ;; have a shorter idle time delay
  (setq js2-idle-timer-delay 0.1)

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings nil
        js2-strict-missing-semi-warning nil
        js2-strict-trailing-comma-warning nil)

  (defun my-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; enable flycheck
    (flycheck-mode)

    ;; format typescript files using prettier
    (prettier-js-mode)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))

    ;; configure javascript-tide checker to run after your default javascript checker
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))

;; js2-refactor: refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :after js2-mode
  :bind
  (:map js2-mode-map
        ("C-k" . js2r-kill)
        ("C-c h r" . js2-refactor-hydra/body))
  :hook ((js2-mode . js2-refactor-mode))
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

;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

;; json-snatcher: get the path of any JSON element easily
;; https://github.com/Sterlingg/json-snatcher
(use-package json-snatcher
  :hook ((json-mode . js-mode-bindings))
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path))))

;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :hook (((js2-mode typescript-mode) . indium-interaction-mode)))

;; mocha: emacs mode for running mocha tests
;; https://github.com/scottaj/mocha.el
(use-package mocha
  :after js2-mode
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
(use-package mocha-snippets)

;; Adds the node_modules/.bin directory to the buffer exec_path. E.g. support project local eslint installations.
;; https://github.com/codesuki/add-node-modules-path/tree/master
(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (rjsx-mode . add-node-modules-path)))

;; eslintd-fix: Emacs minor-mode to automatically fix javascript with eslint_d.
;; https://github.com/aaronjensen/eslintd-fix/tree/master
(use-package eslintd-fix)

;; rjsx-mode: A JSX major mode for Emacs
;; https://github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :after js2-mode
  :mode (("\\.jsx$" . rjsx-mode)
         ("components/.+\\.js$" . rjsx-mode))
  :hook (rjsx-mode . (lambda ()
                          (flycheck-mode)
                          (my-tide-setup-hook)
                          (company-mode)
                          (indium-interaction-mode -1)
                          (js2-refactor-mode -1)))
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config (unbind-key "C-c C-l" rjsx-mode-map))

(provide 'setup-js)
