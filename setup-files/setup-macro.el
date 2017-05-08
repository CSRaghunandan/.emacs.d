;; Time-stamp: <2017-05-08 11:29:01 csraghunandan>

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
  ("q" nil :color blue))

(bind-key "C-c h M" 'hydra-macro/body)

(defmacro my-special-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer."
          (declare (indent 1))
          (let ((fname (intern (concat "my-" (symbol-name mode) "-beginning-of-buffer")))
                (mode-map (intern (concat (symbol-name mode) "-mode-map")))
                (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
            `(progn
               (defun ,fname ()
                 (interactive)
                 (let ((p (point)))
                   (goto-char (point-min))
                   ,@forms
                   (when (= p (point))
                     (goto-char (point-min)))))
               (add-hook ',mode-hook
                         (lambda ()
                           (define-key ,mode-map
                             [remap beginning-of-buffer] ',fname))))))

(defmacro my-special-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer."
          (declare (indent 1))
          (let ((fname (intern (concat "my-" (symbol-name mode) "-end-of-buffer")))
                (mode-map (intern (concat (symbol-name mode) "-mode-map")))
                (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
            `(progn
               (defun ,fname ()
                 (interactive)
                 (let ((p (point)))
                   (goto-char (point-max))
                   ,@forms
                   (when (= p (point))
                     (goto-char (point-max)))))
               (add-hook ',mode-hook
                         (lambda ()
                           (define-key ,mode-map
                             [remap end-of-buffer] ',fname))))))

(my-special-beginning-of-buffer dired
  (while (not (ignore-errors (dired-get-filename)))
    (dired-next-line 1)))
(my-special-end-of-buffer dired
  (dired-previous-line 1))

(my-special-beginning-of-buffer occur
  (occur-next 1))
(my-special-end-of-buffer occur
  (occur-prev 1))

(my-special-beginning-of-buffer ibuffer
  (ibuffer-forward-line 1))
(my-special-end-of-buffer ibuffer
  (ibuffer-backward-line 1))

(my-special-beginning-of-buffer org-agenda
  (org-agenda-next-item 1))
(my-special-end-of-buffer org-agenda
  (org-agenda-previous-item 1))

(provide 'setup-macro)
