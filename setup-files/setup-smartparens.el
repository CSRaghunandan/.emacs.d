;; Time-stamp: <2016-12-12 02:40:36 csraghunandan>

;; smartparens - for movement, editing and inserting parenthesis
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :config
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode web-mode org-mode Info-mode)))

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
              (back-quote   . "`")))

  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("C-M-<down>" . sp-backward-down-sexp)
   ("C-M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-H-f" . sp-forward-symbol)
   ("C-H-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-c C-t" . sp-transpose-hybrid-sexp)

   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)

   ("C-c \"" . wrap-with-double-quotes)
   ("C-c _"  . wrap-with-underscores)
   ("C-c `"  . wrap-with-back-quotes)
   ("C-c s s" . sp-select-next-thing))

  (eval-after-load "smartparens" '(diminish 'smartparens-mode "𝐬"))
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)

  (require 'smartparens-config)

  ;; indent with braces for C like languages
  (sp-with-modes '(rust-mode js2-mode css-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET")))))

(provide 'setup-smartparens)

;; smartparens
;; C-[up] -> Go to the end of a function `sp-up-sexp'
;; C-M-[up] -> Go to the beginning of a function `sp-backward-up-sexp'
;; C-k -> will now kill the sexp at point
;; C-M-a -> go to the beginning of a sexp
;; C-M-e -> go to the end of a sexp
;; C-M-p -> previous sexp
;; C-M-n -> next sexp
;; C-M-w -> copy the sexp at point
;; C-c s s -> select the sexp at point
;; C-M-t -> transpose sexp
;; to wrap the sexp with ( or [ use `C-c [' or `C-c ('
;; M-[ unwrap sexp backward
