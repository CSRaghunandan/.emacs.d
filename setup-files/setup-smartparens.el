;; Time-stamp: <2017-01-06 10:51:00 csraghunandan>

;; smartparens - for movement, editing and inserting parenthesis
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :config
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode
                                     web-mode org-mode
                                     Info-mode
                                     erc-mode
                                     term-mode)))

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
   ("M-k"   . sp-kill-whole-line)
   ("C-c R" . sp-rewrap-sexp)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-c s j" . sp-join-sexp)
   ("C-c s s" . sp-split-sexp)

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
  (smartparens-global-strict-mode)

  (require 'smartparens-config)

  ;; indent with braces for C like languages
  (sp-with-modes '(rust-mode js2-mode css-mode web-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))

  (bind-key "C-c h s"
            (defhydra smartparens-hydra (:hint nil)
"
_d_: down           _a_: back-down        _f_: -> sexp    _k_: hyb-kill      _c_-_a_: begin
_e_: up             _u_: back-up          _b_: <- sexp    _K_: kill          _c_-_e_: end
_[_: back-unwrap    _]_: unwrap           _r_: rewrap     _m_: mark            _j_: join
_p_: prev           _n_: next             _c_: copy       _s_: mark-thing      _|_: split
_t_: transpose      _T_: hyb-transpose
"

              ("d" sp-down-sexp)
              ("e" sp-up-sexp)
              ("u" sp-backward-up-sexp)
              ("a" sp-backward-down-sexp)
              ("f" sp-forward-sexp)
              ("b" sp-backward-sexp)
              ("k" sp-kill-hybrid-sexp)
              ("t" sp-transpose-sexp)
              ("T" sp-transpose-hybrid-sexp)
              ("K" sp-kill-sexp)
              ("[" sp-backward-unwrap-sexp)
              ("]" sp-unwrap-sexp)
              ("r" sp-rewrap-sexp)
              ("p" sp-previous-sexp)
              ("n" sp-next-sexp)
              ("j" sp-join-sexp)
              ("|" sp-split-sexp)
              ("c" sp-copy-sexp)
              ("s" sp-select-next-thing :color blue)
              ("m" sp-mark-sexp :color blue)
              ("q" nil "Quit" :color blue))
            smartparens-mode-map)

  (setq sp-show-pair-from-inside t)
  ;; show matching paren instantly
  (setq sp-show-pair-delay 0.01)
  ;; enable smartparens in minibuffer
  (add-hook 'eval-expression-minibuffer-setup-hook (lambda()
                                                     (smartparens-mode)
                                                     (eldoc-mode)))

  ;; no more pair mismatch messages
  (setq sp-message-width nil)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; fix smartparens-strict-mode and hungry-delete conflict
  (dolist (key '( [remap delete-char]
                  [remap delete-forward-char]))

    (define-key smartparens-strict-mode-map key
      '(menu-item "maybe-sp-delete-char" nil
                  :filter (lambda (&optional _)
                            (unless (looking-at-p "[[:space:]\n]")
                              #'sp-delete-char)))))

  (dolist (key '([remap backward-delete-char-untabify]
                 [remap backward-delete-char]
                 [remap delete-backward-char]))

    (define-key smartparens-strict-mode-map key
      '(menu-item "maybe-sp-backward-delete-char" nil
                  :filter (lambda (&optional _)
                            (unless (looking-back "[[:space:]\n]" 1)
                              #'sp-backward-delete-char))))))

(provide 'setup-smartparens)

;; smartparens
;; C-k -> will now kill the sexp at point
;; C-M-p -> previous sexp
;; C-M-n -> next sexp
;; C-M-w -> copy the sexp at point
;; C-M-t -> transpose sexp
;; M-[ unwrap sexp backward
