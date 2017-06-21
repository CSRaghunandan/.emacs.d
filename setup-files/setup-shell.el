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

(provide 'setup-shell)
