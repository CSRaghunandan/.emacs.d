;; Time-stamp: <2017-02-12 13:27:52 csraghunandan>

;; abbrev: expand abbreviations
(use-package abbrev :ensure nil
  :diminish abbrev-mode
  :config
  ;; Silently save abbrevs on quitting emacs
  (setq save-abbrevs 'silently)

  (defconst rag/abbrev-hooks '(prog-mode-hook
                               org-mode-hook)
    "List of hooks of major modes in which abbrev should be enabled.")

  (defun rag/turn-on-abbrev ()
    "Turn on abbrev only for specific modes"
    (interactive)
    (dolist (hook rag/abbrev-hooks)
      (add-hook hook #'abbrev-mode)))
  (rag/turn-on-abbrev)

  ;;Read the abbreviations file on startup
  (quietly-read-abbrev-file)

  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (endless/simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point"))))

  (bind-key* "C-;" 'endless/ispell-word-then-abbrev))

(provide 'setup-abbrev)
