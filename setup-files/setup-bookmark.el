(use-package bm
  :config
  (setq bm-cycle-all-buffers t
        bm-in-lifo-order t
        bm-highlight-style 'bm-highlight-line-and-fringe)
  (setq-default bm-buffer-persistence t)
  :bind* (("s-h" . bm-toggle)
	  ("s-b n" . bm-next)
	  ("s-b p" . bm-previous)))

(provide 'setup-bookmark)
