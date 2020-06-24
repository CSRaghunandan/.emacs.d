;;; setup-sh.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-22 13:17:29 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; configuration for sh-mode

(defvar +sh-builtin-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")

(use-package sh-script
  :straight nil
  :hook ((sh-mode . (lambda ()
                      (setq-local lsp-diagnostic-package ':none)
                      (setq-local flycheck-checker 'sh-shellcheck)
                      (lsp-deferred)
                      (lsp-ui-doc-mode)
                      (company-mode))))
  :hook (sh-mode . flycheck-mode)
  :mode (("\\.aliases\\'" . sh-mode)
         ("crontab.*\\'" . sh-mode)
         ("\\.zunit\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  :config
  (defun my-sh-mode-hook()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (add-hook 'sh-mode-hook 'my-sh-mode-hook)

  ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
  (dolist (pattern '("\\.zsh\\'"
                     "zlogin\\'"
                     "zlogout\\'"
                     "zpreztorc\\'"
                     "zprofile\\'"
                     "zshenv\\'"
                     "zshrc\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

  ;; coloreize shell output
  (setq comint-terminfo-terminal "ansi")

  (defun +sh--match-variables-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while
            (and (setq res
                       (re-search-forward
                        "[^\\]\\(\\$\\)\\({.+?}\\|\\<[a-zA-Z0-9_]+\\|[@*#!]\\)"
                        limit t))
                 (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))

  (defun +sh--match-command-subst-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while
            (and (setq res
                       (re-search-forward "[^\\]\\(\\$(.+?)\\|`.+?`\\)"
                                          limit t))
                 (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))

  ;; recognize function names with dashes in them
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/sh/config.el
  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (font-lock-add-keywords
   'sh-mode `((+sh--match-variables-in-quotes
               (1 'font-lock-constant-face prepend)
               (2 'font-lock-variable-name-face prepend))
              (+sh--match-command-subst-in-quotes
               (1 'sh-quoted-exec prepend))
              (,(regexp-opt +sh-builtin-keywords 'symbols)
               (0 'font-lock-type-face append))))

  (setq sh-indent-after-continuation 'always))

;; shfmt: Reformat shell script code in Emacs using shfmt
;; https://github.com/purcell/emacs-shfmt
(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode))

(provide 'setup-sh)

;; install `shellcheck' for providing linting for shell scripts. It'll
;; automatically be enabled by flycheck if installed
