;; Timestamp: <2017-06-21 21:00:10>

;; configuration for shell mode

(use-package sh-script
  :config
  (defun my-match-variables-in-quotes (limit)
    "Match variables in double-quotes in `sh-mode'."
    (with-syntax-table sh-mode-syntax-table
      (catch 'done
        (while (re-search-forward
                ;; `rx' is cool, mkay.
                (rx (group "$")
                    (group
                     (or (and "{" (+? nonl) "}")
                        (and word-start (+? nonl) word-end))))
                limit t)
          (-when-let (string-syntax (nth 3 (syntax-ppss)))
            (when (= string-syntax 34)
              (throw 'done (point))))))))

  (font-lock-add-keywords
   'sh-mode '((my-match-variables-in-quotes
               (1 'default t)
               (2 font-lock-variable-name-face t)))))

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
