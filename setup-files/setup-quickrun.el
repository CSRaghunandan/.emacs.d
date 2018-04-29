;; Time-stamp: <2018-04-29 14:28:14 csraghunandan>

;; quickrun - Execute editing buffer and show its output quickly.
;; https://github.com/syohex/emacs-quickrun

(use-package quickrun
  :config
  ;; hydra for quickrun
  (bind-key "C-c h q"
            (defhydra hydra-quickrun (:color teal
                                             :hint nil)
"
_u_: compile + run     _c_: compile file                _s_: execute buffer in eshell
_r_: execute region    _e_: execute + replace region    _a_: execute with args
_q_: quit
"
              ("u" quickrun)
              ("r" quickrun-region)
              ("e" quickrun-replace-region)
              ("c" quickrun-compile-only)
              ("a" quickrun-with-arg)
              ("s" quickrun-shell)
              ("q" nil :color blue))))

(provide 'setup-quickrun)
