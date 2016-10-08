;; Time-stamp: <2016-10-08 08:29:17 csraghunandan>

;; aggressive-indent
;; https://github.com/Malabarba/aggressive-indent-mode

;; This makes sure all the lines of code will strictly align to the indentation level.
;; Just one minor caveat, this does not work for major modes that rely on whitespace
;; for indenting code blocks like python or haskell-mode
(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode)

  ;; The variable aggressive-indent-dont-indent-if lets you customize when you don't want indentation to
  ;; happen. For instance, if you think it's annoying that lines jump around in c++-mode
  ;; because you haven't typed the `;' do this :-
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'css-mode 'js2-mode 'rust-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))

  (global-aggressive-indent-mode))

(provide 'setup-aggresive-indent)
