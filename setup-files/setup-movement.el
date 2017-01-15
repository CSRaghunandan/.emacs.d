;; Time-stamp: <2017-01-15 12:55:32 csraghunandan>

;; All the configuration related to movement in emacs

(defun rag/multi-pop-to-mark (orig-fun &rest args)
  "When popping the mark, continue popping until the cursor actually moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around #'rag/multi-pop-to-mark)

;; Ensure that we can quickly pop the mark several times by typing
;; C-u C-SPC C-SPC, instead of having to type C-u C-SPC C-u C-SPC.
(setq set-mark-command-repeat-pop t)

;; hydra for movement keys
(defhydra hydra-move
  (:body-pre (next-line)
             :hint nil)
  "
_f_: -> char        _F_: -> word         _n_: -> line       _a_: beginning-of-line
_b_: <- char        _B_: <- word         _p_: <- line       _e_: end-of-line

_m_: set mark       _v_: scroll down     _l_: recenter      _'_: avy       _`_: avy-word
_j_: goto mark      _V_: scroll up       _w_: ace-window    _._: -> buffer _,_: <- buffer

_s_: -> sentence    _a_: -> paragraph    _g_: -> page       _>_: end-of-buffer
_S_: <- sentence    _A_: <- paragraph    _G_: <- page       _<_: beginning-of-buffer
 "
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" mwim-beginning-of-code-or-line)
  ("e" move-end-of-line)
  ("v" scroll-up-command)
  ("V" scroll-down-command)
  ("F" forward-word)
  ("B" backward-word)
  ("l" recenter-top-bottom)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("g" forward-page)
  ("G" backward-page)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("a" forward-paragraph)
  ("A" backward-paragraph)
  ("'" avy-goto-char-timer)
  ("`" avy-goto-word-1)
  ("w" ace-window)
  ("m" org-mark-ring-push)
  ("j" org-mark-ring-goto)
  ("." next-buffer)
  ("," previous-buffer)
  ("q" nil :color blue))
(bind-key "M-m" 'hydra-move/body)

;; move to the beginning or end of line smartly
;; https://github.com/alezost/mwim.el
(use-package mwim
  :bind (:map prog-mode-map
                ("C-a" . mwim-beginning-of-code-or-line)
                ("C-e" . mwim-end-of-code-or-line)))

(provide 'setup-movement)
