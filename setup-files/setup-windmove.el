;; hydra for window management

;; split window and move there.
(defun rag/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun rag/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; toggle frame fullscreen
(defun rag/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

(defhydra hydra-window (:color red
                               :hint nil)
  "
 ^Move^    ^Size^    ^Change^                    ^Split^           ^Text^
 ^^^^^^^^^^^------------------------------------------------------------------
 ^ ^ _w_ ^ ^   ^ ^ _W_ ^ ^   _u_: winner-undo  _p_: swap   _v_: vertical     _+_: zoom in
 _a_ ^+^ _d_   _A_ ^+^ _D_   _r_: winner-redo            _h_: horizontal   _-_: zoom out
 ^ ^ _s_ ^ ^   ^ ^ _S_ ^ ^   _x_: close                  _z_: zoom         _q_: quit
"
  ("a" windmove-left)
  ("s" windmove-down)
  ("d" windmove-right)
  ("w" windmove-up)
  ("A" shrink-window-horizontally)
  ("W" shrink-window)
  ("S" enlarge-window)
  ("D" enlarge-window-horizontally)
  ("h" rag/split-right-and-move)
  ("v" rag/split-below-and-move)
  ("x" delete-window)
  ("p" ace-swap-window)
  ("z" delete-other-windows)
  ("f" rag/toggle-frame-fullscreen-non-native :color blue)
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :color blue))

(bind-keys*
 ("C-x 2" . rag/split-below-and-move)
 ("C-x 3" . rag/split-right-and-move)
 ("C-x o" . hydra-window/body))

(provide 'setup-windmove)
