;;; setup-compile.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-04 22:04:16 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t)

;; https://gitlab.com/jgkamat/rmsbolt
;; rmsbolt:A godbolt embedded in Emacs
(use-package rmsbolt
  :defer t)

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
              ("q" nil "Quit" :color blue))))

(provide 'setup-compile)
