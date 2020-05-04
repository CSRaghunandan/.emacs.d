;;; setup-macro.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-04 22:04:29 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; hydra for macros in emacs
(defhydra hydra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                               (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_p_^           [_e_] execute    [_i_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _b_ ←   → _f_     [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_F_] format    [_B_] defun
     ^_n_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("b" kmacro-start-macro :color blue)
  ("f" kmacro-end-or-call-macro-repeat)
  ("p" kmacro-cycle-ring-previous)
  ("n" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("i" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("F" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil "Quit" :color blue))

(bind-key "C-c h M" 'hydra-macro/body)

(provide 'setup-macro)
