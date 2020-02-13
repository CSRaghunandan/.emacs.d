;;; setup-smartparens.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-13 15:32:49 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; smartparens: for movement, editing and inserting parenthesis
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :config
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode
                                     Info-mode
                                     term-mode
                                     org-mode
                                     org-journal-mode
                                     markdown-mode
                                     ivy-occur-mode)))

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
  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))

  (bind-keys
   :map smartparens-mode-map
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-S-d" . sp-beginning-of-sexp)
   ("C-S-a" . sp-end-of-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("M-P" . sp-previous-sexp)
   ("M-N" . sp-next-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-<delete>" . sp-unwrap-sexp)
   ("M-<backspace>" . sp-backward-unwrap-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)
   ("M-D" . sp-splice-sexp)
   ("C-M-<delete>" . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ("C-S-<backspace>" . sp-splice-sexp-killing-around)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-M-]" . sp-select-next-thing)
   ("C-M-SPC" . sp-mark-sexp)
   ("M-F" . sp-forward-symbol)
   ("M-B" . sp-backward-symbol)

   ("C-c R" . sp-rewrap-sexp)
   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-c s j" . sp-join-sexp)
   ("C-c s s" . sp-split-sexp)

   ("C-c )" . wrap-with-parens)
   ("C-c ]" . wrap-with-brackets)
   ("C-c }" . wrap-with-braces)
   ("C-c '" . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c `" . wrap-with-back-quotes))

  ;; enable smartparens globally
  (smartparens-global-mode)
  (smartparens-global-strict-mode) ; only allows you to insert or delete
                                   ; brackets in pairs
  (show-smartparens-global-mode +1)

  (require 'smartparens-config)

  ;; indent with braces for C like languages
  (sp-with-modes '(rustic-mode
                   js2-mode
                   css-mode
                   web-mode
                   typescript-mode
                   c-mode
                   c++-mode
                   sh-mode
                   go-mode
                   shell-mode)
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
_t_: transpose      _T_: hyb-transpose    _q_: quit
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
              ("q" nil :color blue))
            smartparens-mode-map)

  (setq sp-show-pair-from-inside t)
  ;; show matching paren instantly
  (setq sp-show-pair-delay 0.1)

  ;; no more pair mismatch messages
  (setq sp-message-width nil)

  (defun sp-strict-kill-line-or-region (&optional arg)
    "Kill active region or current line."
    (interactive "p")
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-kill-whole-line)))

  (bind-key* "C-w" #'sp-strict-kill-line-or-region smartparens-mode-map))

(provide 'setup-smartparens)

;; smartparens
;; C-k -> will now kill the sexp at point
;; C-M-p -> previous sexp
;; C-M-n -> next sexp
;; C-M-w -> copy the sexp at point
;; C-M-t -> transpose sexp
;; M-[ unwrap sexp backward
