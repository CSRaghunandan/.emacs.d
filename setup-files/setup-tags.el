;; Time-stamp: <2016-10-10 18:55:44 csraghunandan>

;; setup tags for emacs

;;; gtags, GNU global

;; ggtags
;; https://github.com/leoliu/ggtags
(when (executable-find "global")
  (use-package ggtags :defer t))

(provide 'setup-tags)
