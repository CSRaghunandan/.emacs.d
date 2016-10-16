;; Time-stamp: <2016-10-16 15:05:58 csraghunandan>

;; recursive narrow or widen buffers
;; https://github.com/nflath/recursive-narrow/blob/master/recursive-narrow.el

(use-package recursive-narrow
  :config
  (require 'recursive-narrow))

(provide 'setup-recursive-narrow)

;; press `C-x n w' when inside a recursive narrow to widen the buffer back to the
;; narrowed buffer instead of widening to the whole buffer
