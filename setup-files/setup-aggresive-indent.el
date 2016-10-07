;; Time-stamp: <2016-10-06 22:28:45 csraghunandan>

;; aggressive-indent
;; https://github.com/Malabarba/aggressive-indent-mode

;; This makes sure all the lines of code will strictly align to the indentation level.
;; Just one minor caveat, this does not work for major modes that rely on whitespace
;; for indenting code blocks like python or haskell-mode
(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode)
  (global-aggressive-indent-mode))

(provide 'setup-aggresive-indent)
