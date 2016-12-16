;; Time-stamp: <2016-12-16 23:29:31 csraghunandan>

;; smartparens - for movement, editing and inserting parenthesis
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :config
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode web-mode org-mode Info-mode erc-mode)))

  (bind-keys
   :map smartparens-mode-map
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp))

  (eval-after-load "smartparens" '(diminish 'smartparens-mode "𝐬"))
  (show-smartparens-global-mode +1)
  ;; use default smartparens bindings
  (sp-use-smartparens-bindings)
  (smartparens-global-mode 1)

  (require 'smartparens-config)

  ;; indent with braces for C like languages
  (sp-with-modes '(rust-mode js2-mode css-mode)
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
            smartparens-mode-map))

(provide 'setup-smartparens)

;; smartparens
;; C-k -> will now kill the sexp at point
;; C-M-p -> previous sexp
;; C-M-n -> next sexp
;; C-M-w -> copy the sexp at point
;; C-M-t -> transpose sexp
;; M-[ unwrap sexp backward
