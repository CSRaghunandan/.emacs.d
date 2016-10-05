;; configure bookmark

(use-package bm
  :config
  (setq bm-cycle-all-buffers t
        bm-in-lifo-order t
        bm-highlight-style 'bm-highlight-line-and-fringe)
  (setq-default bm-buffer-persistence t)

  (defun rag/bm-bookmark-regexp ()
    (interactive)
    (if (use-region-p)
        (progn
          (bm-bookmark-regexp-region (region-beginning) (region-end))
          (deactivate-mark))
      (bm-bookmark-regexp)))

  (defhydra hydra-bm (:color pink
                             :hint nil
                             :body-pre (when (not (use-region-p)) (push-mark)))
    "
Bookmark _n_ext (_N_ in lifo order)            toggle book_m_ark        ^^_/_ bm lines matching regexp                          toggle per_s_istence
         _p_revious (_P_ in lifo order)        _a_nnotate               _x_/_X_ remove all bm from current/all buffer(s)        _r_eturn to from where you started
    "
    ("m"   bm-toggle)
    ("M"   bm-toggle :color blue)
    ("a"   bm-bookmark-annotate :color blue)
    ("n"   bm-common-next)
    ("N"   bm-lifo-next)
    ("p"   bm-common-previous)
    ("P"   bm-lifo-previous)
    ("/"   rag/bm-bookmark-regexp :color blue)
    ("s"   bm-toggle-buffer-persistence)
    ("x"   bm-remove-all-current-buffer :color blue)
    ("X"   bm-remove-all-all-buffers :color blue)
    ("r"   pop-to-mark-command :color blue)
    ("RET" nil "cancel" :color blue)
    ("q"   nil "cancel" :color blue))
  (bind-key* "s-h" 'hydra-bm/body))

(provide 'setup-bookmark)
