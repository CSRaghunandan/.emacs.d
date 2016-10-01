(defun smarter-move-beginning-of-line (arg)
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

;; split window and move there.
(defun sk/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun sk/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "When popping the mark, continue popping until the cursor actually moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around #'modi/multi-pop-to-mark)

;; hydra for movement keys
(defhydra hydra-move
  (:body-pre (next-line))
  "move"
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("v" scroll-up-command)
  ("k" forward-word)
  ("j" backward-word)
  ;; Converting M-v to V here by analogy.
  ("V" scroll-down-command)
  ("l" recenter-top-bottom))
(bind-key "M-m" 'hydra-move/body)

(bind-keys*
 ("C-x 2" . sk/split-below-and-move)
 ("C-x 3" . sk/split-right-and-move)
 ("C-a" . smarter-move-beginning-of-line))

(provide 'setup-movement)
