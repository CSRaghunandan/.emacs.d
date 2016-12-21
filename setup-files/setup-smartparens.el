;; Time-stamp: <2016-12-21 15:46:11 csraghunandan>

;; smartparens - for movement, editing and inserting parenthesis
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :config
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode web-mode org-mode Info-mode erc-mode)))

  ;; macro to wrap the current sexp at point
  (defmacro def-pairs (pairs)
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))
  (def-pairs ((paren        . "(")
              (bracket      . "[")
              (brace        . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))

  (bind-keys
   :map smartparens-mode-map
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-c R" . sp-rewrap-sexp)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-c )"  . wrap-with-parens)
   ("C-c ]"  . wrap-with-brackets)
   ("C-c }"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c `" . wrap-with-back-quotes))

  (eval-after-load "smartparens" '(diminish 'smartparens-mode "𝐬"))
  (show-smartparens-global-mode +1)
  ;; use default smartparens bindings
  (sp-use-smartparens-bindings)
  (smartparens-global-mode 1)

  (require 'smartparens-config)

  ;; indent with braces for C like languages
  (sp-with-modes '(rust-mode js2-mode css-mode web-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))

  (bind-key "C-M-s"
            (defhydra smartparens-hydra ()
              "Smartparens"
              ("d" sp-down-sexp "Down")
              ("e" sp-up-sexp "Up")
              ("u" sp-backward-up-sexp "Up")
              ("a" sp-backward-down-sexp "Down")
              ("f" sp-forward-sexp "Forward")
              ("b" sp-backward-sexp "Backward")
              ("k" sp-kill-sexp "Kill" :color blue)
              ("q" nil "Quit" :color blue))
            smartparens-mode-map)

  ;; do not insert paris when point is at the beginning of word
  ;; https://emacs.stackexchange.com/questions/26912/smartparens-do-not-insert-parenthesis-pair-when-point-is-at-the-beginning-of-wo
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p))
  (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))

  (setq sp-show-pair-from-inside t)
  ;; show matching paren instantly
  (setq sp-show-pair-delay 0.01))

(provide 'setup-smartparens)

;; smartparens
;; C-k -> will now kill the sexp at point
;; C-M-p -> previous sexp
;; C-M-n -> next sexp
;; C-M-w -> copy the sexp at point
;; C-M-t -> transpose sexp
;; M-[ unwrap sexp backward
