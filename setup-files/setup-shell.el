;; Timestamp: <2017-06-21 21:00:10>

;; configuration for shell mode

(defvar +sh-builtin-keywords
  '("cat" "cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git"
    "grep" "head" "kill" "less" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd"
    "rm" "sleep" "sudo" "tail" "tee" "touch")
  "A list of common shell commands and keywords to be fontified especially in
`sh-mode'.")

(use-package sh-script
  :config
  (defun +sh--match-variables-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while
            (and (setq res
                       (re-search-forward
                        "\\(\\$\\)\\({.+?}\\|\\<.+?\\>\\)"
                        limit t))
                 (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))

  (defun +sh--match-command-subst-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while
            (and (setq res
                       (re-search-forward "\\(\\$(.+?)\\|`.+?`\\)"
                                          limit t))
                 (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (font-lock-add-keywords
   'sh-mode `((+sh--match-variables-in-quotes
               (1 'default prepend)
               (2 'font-lock-variable-name-face prepend))
              (+sh--match-command-subst-in-quotes
               (0 'sh-quoted-exec prepend))
              (,(regexp-opt +sh-builtin-keywords 'words)
               (0 'font-lock-builtin-face append))))

  (setq sh-indent-after-continuation 'always))

;; company-shell: company backend for shell scripts
;; https://github.com/Alexander-Miller/company-shell
(use-package company-shell
  :after sh-script
  :config
  (setq company-shell-delete-duplicates t)
  (defun my-sh-mode-hook()
    (set (make-local-variable 'company-backends)
         '(company-shell company-files company-yasnippet)))
  (add-hook 'sh-mode-hook #'my-sh-mode-hook)
  (add-hook 'sh-mode-hook 'company-mode))

(provide 'setup-shell)
