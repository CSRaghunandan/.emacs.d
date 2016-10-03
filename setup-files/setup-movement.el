(defun rag/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun rag/multi-pop-to-mark (orig-fun &rest args)
  "When popping the mark, continue popping until the cursor actually moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around #'rag/multi-pop-to-mark)

;; hydra for movement keys
(defhydra hydra-move
  (:body-pre (next-line)
             :hint nil)
  "
_f_: -> char        _F_: -> word         _n_: -> line       _a_: beginning-of-line
_b_: <- char        _B_: <- word         _p_: <- line       _e_: end-of-line

_m_: set mark       _v_: scroll down     _l_: recenter      _'_: avy
_j_: goto mark      _V_: scroll up       _w_: ace-window    _._: -> buffer _,_: <- buffer

_s_: -> sentence    _a_: -> paragraph    _g_: -> page       _>_: end-of-buffer
_S_: <- sentence    _A_: <- paragraph    _G_: <- page       _<_: beginning-of-buffer
 "
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" rag/smarter-move-beginning-of-line)
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
  ("w" ace-window)
  ("m" org-mark-ring-push)
  ("j" org-mark-ring-goto)
  ("." next-buffer)
  ("," previous-buffer)
  ("q" nil "quit" :color blue))
(bind-key "M-m" 'hydra-move/body)

(bind-key* "C-a" 'rag/smarter-move-beginning-of-line)

(provide 'setup-movement)
